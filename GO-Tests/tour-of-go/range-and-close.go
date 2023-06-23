package main;

func fibonacci(n int, c chan int) {
  x := 0; y := 1;
  for i := 0; i < n; i++ {
    c <- x;
    y = x + y;
    x = y - x;
  };
};

func main() {
  c := make(chan int, 10);
  go fibonacci(10, c);
  for i := 0; i < 10; i++ {
    println(<-c);
  };
};
