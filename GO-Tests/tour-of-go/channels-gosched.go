package main;

import "runtime"

func sum(n int, in chan int, out chan int) {
  runtime.Gosched();
  sum := 0;
  runtime.Gosched();
  for i := 0; i < n; i++ {
    runtime.Gosched();
    sum += <-in;
    runtime.Gosched();
  };
  runtime.Gosched();
  out <- sum;
  runtime.Gosched();
};

func main() {
  c1 := make(chan int, 3);
  c2 := make(chan int, 3);
  c3 := make(chan int);

  c1 <-  7; c1 <- 2; c1 <- 8;
  c2 <- -9; c2 <- 4; c2 <- 0;

  go sum(3, c1, c3);
  runtime.Gosched();
  go sum(3, c2, c3);
  runtime.Gosched();

  x := <-c3;
  runtime.Gosched();
  y := <-c3;
  runtime.Gosched();

  println(x, y, x+y);
};
