import java.util.Scanner;

public class Fac{

    public static void main(String[] args) {
        System.out.println(facto(1000000));
    }
    
    public static int factorial(int numero) {
		if (numero==0)
			return 1;
		else
			return numero * factorial(numero-1);
}
}
