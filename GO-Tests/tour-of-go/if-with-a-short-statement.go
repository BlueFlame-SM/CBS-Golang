package main;

func math_Pow(x int, n int) int {
  res := 1;
  for i := 0; i < n; i++ {
    res *= x;
  };
  return res;
};

func pow(x int, n int, lim int) int {
  if v := math_Pow(x, n); v < lim {
    return v;
  };
  return lim;
};

func main() {
  println(
    pow(3, 2, 10),
    pow(3, 3, 20),
  );
};
