package main;

func main() {
  c := make(chan int, 10);
  for i:=0; i < 10; i++ {
    c <- i;
  };

  for i:=0; i < 10; i++ {
    print(<-c);
  };
};
