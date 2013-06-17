int plus (int x, int y) {
   return x + y;
 }

 // Sum an array of integers
 int sum (int *array, int len) {
   int n;
   int sum = 0;
   for (n = 0; n < len; n++)
     sum += array[n];
   return sum;
}


