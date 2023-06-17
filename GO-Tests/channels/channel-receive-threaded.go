package main;

func main() {
  c := make(chan int, 10);
  go thread(c);
  for i := 1; i < 10; i++ {
    print(<-c);
  };
};

func thread(c any) {
  for i := 1; i < 10; i++ {
    c <- i;
  };
};
