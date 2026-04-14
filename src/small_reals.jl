import Lazy

abstract type SmallHalfInteger end
abstract type SignedHalfBit <: SmallHalfInteger end

abstract type SignedBit <: SignedHalfBit end

struct ThreeHalves <: SmallHalfInteger end
struct One <: SignedBit end
struct Half <: SignedHalfBit end
struct Zero <: SignedBit end
struct NegHalf <: SignedHalfBit end
struct NegOne <: SignedBit end
struct NegThreeHalves <: SmallHalfInteger end

toString(::One) = "1"
toString(::Zero) = "0"
toString(::NegOne) = "n"


Base.show(io::IO, x::SignedBit) = print(io, toString(x))

Base.:-(::One) = NegOne()
Base.:-(::Zero) = Zero()
Base.:-(::NegOne) = One()

SmallReal = Lazy.List

Base.:-(x::SmallReal) =  Lazy.map(-, x)


bit_average(first::SignedBit, second::SignedBit) = bit_average(second, first) # commutative
bit_average(::T, ::T) where T<:SignedBit = T()
bit_average(::NegOne, ::One) = Zero()
bit_average(::Zero, ::One) = Half()
bit_average(::NegOne, ::Zero) = NegHalf()

head(xs::SmallReal) = Lazy.isempty(xs) ?  Zero() : Lazy.first(xs)
tail(xs::SmallReal) = Lazy.isempty(xs) ? SmallReal() : Lazy.tail(xs)

average_half_bits(xs::SmallReal, ys::SmallReal) = Lazy.@lazy bit_average(head(xs), head(ys)) : average_half_bits(tail(xs), tail(ys))


Base.:+(::Zero, h::Zero) = h
Base.:+(::Zero, h::SignedHalfBit) = h
Base.:+(b::SignedBit, ::Zero) = b
Base.:+(::One, x::One) = error("$x is out of range of SmallHalfInteger")
Base.:+(::One, ::NegOne) = Zero()
Base.:+(::One, ::Half) = ThreeHalves()
Base.:+(::One, ::NegHalf) = Half()
Base.:+(::NegOne, ::One) = Zero()
Base.:+(::NegOne, x::NegOne) = error("$x is out of range of SmallHalfInteger")
Base.:+(::NegOne, ::Half) = NegHalf()
Base.:+(::NegOne, ::NegHalf) = NegThreeHalves()


bit_and_carry(target::SignedBit, ::SignedHalfBit) = (target, Zero())
bit_and_carry(::ThreeHalves, ::SignedHalfBit) = (One(), One())
bit_and_carry(::Half, ::SignedHalfBit) = (Zero(), One())
bit_and_carry(::Half, ::One) = (One(), NegOne())
bit_and_carry(::Half, ::Half) = (One(), NegOne())
bit_and_carry(::NegHalf, ::SignedHalfBit) = (Zero(), NegOne())
bit_and_carry(::NegHalf, ::NegOne) = (NegOne(), One())
bit_and_carry(::NegHalf, ::NegHalf) = (NegOne(), One())
bit_and_carry(::NegThreeHalves, ::SignedHalfBit) = (NegOne(), NegOne())

function dehalf_bits(xs::Lazy.List, carry::SignedBit = Zero())
    target = carry + head(xs)
    rest = tail(xs)
    (bit, carry) = bit_and_carry(target, head(rest))
    return Lazy.@lazy bit : dehalf_bits(rest, carry)
end

average(xs::SmallReal, ys::SmallReal) = dehalf_bits(average_half_bits(xs, ys))

Base.:+(::Zero, xs::SmallReal) = xs
function Base.:+(b::SignedBit, xs::SmallReal)
    if head(xs) == -b
        return b:tail(xs)
    elseif head(xs) isa Zero
        return Lazy.@lazy b : (b + tail(xs))
    elseif head(xs) == b
        rest = tail(xs)
        (head(rest) != -b) && return error("$b + $xs is out of range of SmallReal")
        return b + (Zero() : b : tail(rest))
    end
end
Base.:+(xs::SmallReal, b::SignedBit) = b + xs

function Base.:+(xs::SmallReal, ys::SmallReal)
    half_sum = average(xs, ys)
    try
        return head(half_sum) + tail(half_sum)
    catch
        return error("The sum of $xs and $ys is out of range of SmallReal")
    end
end

zeroes = Lazy.constantly(Zero())

Base.:*(::Zero, ::SmallReal) = zeroes
Base.:*(::One, xs::SmallReal) = xs
Base.:*(::NegOne, xs::SmallReal) = -xs

function product_loop(xs::SmallReal, ys::SmallReal, acc::SmallReal)
    function against_zero(xs::SmallReal)
        first_bit = head(xs)
        !(first_bit isa Zero) && return Lazy.@lazy first_bit : against_zero(tail(xs))
        rest = tail(xs)
        second_bit = head(rest)
        return Lazy.@lazy second_bit : against_zero(-second_bit : tail(rest))
    end
    
    partial_sum = against_zero(acc + (Zero() : head(xs) * ys))
    return Lazy.@lazy head(partial_sum) : product_loop(tail(xs) , ys, tail(partial_sum))
end

times(xs::SmallReal, ys::SmallReal) = product_loop(xs, ys, zeroes)

function Base.abs(r::SmallReal)
    first_bit = head(r)
    first_bit isa Zero && return Lazy.@lazy Zero() : abs(tail(r))
    return first_bit * r
end

