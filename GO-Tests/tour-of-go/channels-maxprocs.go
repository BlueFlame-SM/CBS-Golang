package main;

import "runtime"

func sum(n int, in chan int, out chan int) {
  sum := 0;
  for i := 0; i < n; i++ {
    sum += <-in;
  };
  out <- sum;
};

func main() {
  runtime.GOMAXPROCS(3);
  c1 := make(chan int, 3);
  c2 := make(chan int, 3);
  c3 := make(chan int);

  c1 <-  7; c1 <- 2; c1 <- 8;
  c2 <- -9; c2 <- 4; c2 <- 0;

  go sum(3, c1, c3);
  go sum(3, c2, c3);

  x := <-c3;
  y := <-c3;

  println(x, y, x+y);
};
