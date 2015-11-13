package parameters;

public class Dependencies {

	static class Type1<V extends Type1<V>> {		
	}

	static class Type2<T extends Type2<T>> extends Type1<Type2<T>> {
	}
	
	static class Type3 extends Type2<Type3> {
	}

	Type1<Type2<Type3>> f1;  

	static class Type4<V extends Type4.Type5<V>>  {

		static class Type5<V1 extends Type4.Type5<V1>> {  
		}

		Type4.Type5<V> f2;  
	
	}

}
