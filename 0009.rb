max = 1000

# from to 1, 2, 997 to 331, 334, 335 or 332, 333, 335

max_fst = (max / 3.0).floor - 1

tris = []

1.upto(max_fst) do |i|
  half = (1000 - i) / 2.0
  max_snd =
    if half.ceil > half.floor
      half.floor
    else
      half.to_i - 1
    end
  (i + 1).upto(max_snd) do |j|
    tris << [i, j, max - i - j]
  end
end

tris.each do |fst, snd, trd|
  if fst.**(2) + snd.**(2) == trd.**(2)
    puts fst * snd * trd
    exit
  end
end
