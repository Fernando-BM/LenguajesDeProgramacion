import java.util.Scanner;

public class Factorial {

    public static void main(String[] args) {
        System.out.println(facto(500000000));
    }
    
    public static int facto(int numero) {
		if (numero==0)
			return 1;
		else
			return numero * facto(numero-1);
}
}
