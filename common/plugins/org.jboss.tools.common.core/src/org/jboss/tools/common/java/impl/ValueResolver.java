/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.java.impl;

import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IPackageDeclaration;
import org.eclipse.jdt.core.ISourceReference;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.compiler.env.ISourceField;
import org.eclipse.jdt.internal.core.JavaElement;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.util.EclipseJavaUtil;
import org.jboss.tools.common.util.StringUtil;

/**
 * IField.getConstant() and IMemberValuePair.getValue() return non-null object
 * only for trivial values, even a simple expression 2 + 2 results in null.
 * 
 * There is a way out, to build AST expression and resolve it, but that requires 
 * building AST tree for the entire compilation unit, so that if a client model
 * avoids AST for the sake of performance, it is unacceptable.
 * 
 * This class provides resolution of primitive type expressions.
 * 
 * Supported:
 * - references to static final fields	-	MyType.MY_CONSTANT;
 * - numerics, strings and characters	-	1 2L 2.0 3.14D "a string" 'c';
 * - conversion between primitive types	-	(short)3L;
 * - arithmetic operations, parenthesis	-	(2 + 3) * (MyType.MY_CONSTANT + 1) / (2 - 1);
 * Not supported:
 * - method and constructor calls.
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class ValueResolver {
	private IJavaElement element;
	private Object constant = null;
	private ReferenceResolver referenceResolver = null;

	public ValueResolver(IJavaElement element) {
		this.element = element;
	}

	/**
	 * Constant is computed at each call to resolvePair(IMemberValuePair).
	 * It is to be requested before resolving next pair.
	 * @return
	 */
	public Object getConstant() {
		return constant;
	}

	/**
	 * Call this method after having used to resolve all expressions 
	 * and/or members. When resolving a reference to a constant, 
	 * the implementation may modify compilation unit copy to get 
	 * access to type resolution. This copy must be discarded.
	 */
	public void dispose() {
		if(referenceResolver != null) {
			referenceResolver.dispose();
			referenceResolver = null;
		}
	}

	/**
	 * For a complex expression returns source string if it is 
	 * available while the result of calculation is to be requested 
	 * by getConstant().
	 * 
	 * For a reference to a constant returns resolved qualified name
	 * while the constant value is to be requested by getConstant().
	 * 
	 * Otherwise, returns pair.getValue(). If the value is an array,
	 * then for each element that is a reference to a constant
	 * that element is replaced with resolved qualified name 
	 * of the reference.
	 * 
	 * @param pair
	 * @return
	 */
	public Object resolvePair(IMemberValuePair pair) {
		constant = null;
		Object value = pair.getValue();
		int k = pair.getValueKind();
		if(k == IMemberValuePair.K_QUALIFIED_NAME || k == IMemberValuePair.K_SIMPLE_NAME
			|| (value instanceof Object[] && k == IMemberValuePair.K_UNKNOWN)) {
			if(element != null && element.getAncestor(IJavaElement.COMPILATION_UNIT) instanceof ICompilationUnit) {
				value = resolve(value);
			}
		} else if(k == IMemberValuePair.K_UNKNOWN && value == null) {
			if(element instanceof ISourceReference) {
				try {
					String source = getExpressionForName(pair.getMemberName());
					if(source != null) {
						Object c = resolveExpression(source);
						if(c != null) {
							value = source;
							constant = c;
						}
					}
				} catch (CoreException e) {
					CommonCorePlugin.getPluginLog().logError(e);
				}
			}
		}
		return value;
	}

	private String getExpressionForName(String name) throws CoreException {
		if(name == null) {
			name = AnnotationDeclaration.VALUE;
		}
		String source = getExpression();
		if(source != null) {
			if(source.indexOf('=') < 0) {
				if(AnnotationDeclaration.VALUE.equals(name)) {
					return source;
				} else {
					return null;
				}
			}
			StringTokenizer st = new StringTokenizer(source, ",");
			while(st.hasMoreTokens()) {
				String t = st.nextToken().trim();
				int i = t.indexOf('=');
				if(i < 0) continue;
				if(t.substring(0, i).trim().equals(name)) {
					return t.substring(i + 1).trim();
				}
			}
		}
		return null;
	}

	/**
	 * Simple resolve when JDT resolved value and knows value type.
	 * @param value
	 * @return
	 */
	private Object resolve(Object value) {
		if(value instanceof Object[]) {
			Object[] vs = (Object[])value;
			for (int i = 0; i < vs.length; i++) {
				vs[i] = resolve(vs[i]);
			}
			constant = null; // getConstant() would return not array but one of its values.
		} else if (value != null && isNameToken(value.toString())) {
			try {
				if(connect()) {
					value = referenceResolver.resolveReference(value);
				}
			} catch (CoreException e) {
				CommonCorePlugin.getPluginLog().logError(e);
			}
		}		
		return value;
	}

	private String getExpression() throws CoreException {
		String source = ((ISourceReference)element).getSource();
		if(source != null) {
			int b = source.indexOf('(');
			int e = source.lastIndexOf(')');
			if(b > 0 && e > b) {
				return source.substring(b + 1, e).trim();
			}
		}
		return null;
	}

	private void setFieldInitialValueToConstant(IField f) throws JavaModelException {
		Object c = getFieldInitialValue(f);
		if(c != null) {
			if(c instanceof String) {
				constant = StringUtil.trimQuotes(c.toString());
			} else if(c instanceof Number || c instanceof Boolean) {
				constant = c;
			} else {
				constant = c.toString();
			}
		}
	}

	/**
	 * Returns calculated initial value of field.
	 * @param f
	 * @return
	 * @throws JavaModelException
	 */
	public static Object getFieldInitialValue(IField f) throws JavaModelException {
		Object c = f.getConstant();
		if(c == null && (((JavaElement)f).getElementInfo() instanceof ISourceField)) {
			char[] cs = ((ISourceField)((JavaElement)f).getElementInfo()).getInitializationSource();
			if(cs != null) {
				ValueResolver r = new ValueResolver(f);
				c = r.resolveExpression(new String(cs));
				r.dispose();
			}
		}
		return c;
	}

	public Object resolveExpression(String expression) {
		Expression expr = new Expression(expression, 0, expression.length());
		try {
			return expr.compute();
		} catch (WrongExpressionException exc) {
			//ignore - user input
		}
		return null;
	}

	private boolean connect() throws CoreException {
		if(referenceResolver == null) {
			referenceResolver = new ReferenceResolver();
		}
		return referenceResolver.connect();
	}

	static class WrongExpressionException extends Exception {
		public WrongExpressionException(String message) {
			super(message);
		}
	}
	
	class Expression {
		String expression;
		int from;
		int to;
		int index;
		Object result = null;

		public Expression(String expression, int from, int to) {
			this.expression = expression;
			this.from = from;
			this.to = to;
			index = from;
			skipSpaces();
		}
		void skipSpaces() {
			while(index < to && Character.isWhitespace(expression.charAt(index))) {
				index++;
			}
		}
		int getOperandTokenEnd() {
			if(index == to) {
				return index;
			}
			for (int i = index; i < to; i++) {
				char ch = expression.charAt(i);
				if(!Character.isJavaIdentifierPart(ch) && ch != '.') {
					return i;
				}
			}
			return to;
		}

		public Object compute() throws WrongExpressionException {
			Object left = computeOperand();
			skipSpaces();
			if(index == to) {
				result = left;
			} else {
				char ch = expression.charAt(index);
				while(ch == '*' || ch == '/') {
					index++;
					Object right = computeOperand();
					skipSpaces();
					if(ch == '*') {
						left = multiply(left, right);
					} else if(ch == '/') {
						left = divide(left, right);
					}
					if(index == to) {
						result = left;
						return result;						
					} else {
						ch = expression.charAt(index);
					}
					
				}
				if(ch == '+') {
					index++;
					Object right = compute();
					return add(left, 0, right);
				} else if(ch == '-') {
					Object right = compute();
					return add(left, 0, right);
				}
			}
			return result;
		}
		
		Object computeOperand() throws WrongExpressionException {
			skipSpaces();
			if(index == to) {
				throw new WrongExpressionException("Operand expected");
			}
			char ch = expression.charAt(index);
			if(ch == '"') {
				int m = findMatchingQuote(expression, index, to);
				if(m < 0) throw new WrongExpressionException("Quote does not match.");
				int b = index + 1;
				index = m;
				return expression.substring(b, m - 1);
			} else if(ch == '\'') {
				int m = findMatchingQuote(expression, index, to);
				if(m < 0) throw new WrongExpressionException("Quote does not match.");
				int b = index + 1;
				index = m;
				String v = expression.substring(b, m - 1);
				if(v.length() == 1) {
					return new Character(expression.charAt(b));
				} else if(v.startsWith("\\")) {
					if(v.equals("\\n")) return new Character('\n');
					if(v.equals("\\r")) return new Character('\r');
					if(v.equals("\\t")) return new Character('\t');
					if(v.equals("\\b")) return new Character('\b');
					if(v.equals("\\f")) return new Character('\f');
					if(v.equals("\\'")) return new Character('\'');
					if(v.equals("\\\"")) return new Character('"');
					if(v.equals("\\\\")) return new Character('\\');
					if(v.startsWith("\\u") && v.length() == 6) {
						try {
							return new Character((char)Integer.parseInt(v.substring(2), 16));
						} catch (NumberFormatException e) {
							//ignore - user input
						}
					}
				}
				throw new WrongExpressionException("Not supported character " + v);
			} else if(ch == '(') {
				int m = findMatchingBrace(expression, index, to);
				if(m < 0) throw new WrongExpressionException("Braces does not match.");
				String sub = expression.substring(index + 1, m - 1).trim();
				if(sub.length() == 0) throw new WrongExpressionException("Expression expected at " + (index + 1));
				if(PRIMITIVE_TYPES.contains(sub)) {
					index = m;
					Object o = computeOperand();
					if(o instanceof Character) {
						if(!"char".equals(sub)) {
							o = new Integer((int)((Character)o).charValue());
						} else {
							return o;
						}
					}
					if(o instanceof Number) {
						if("int".equals(sub)) {
							return new Integer(((Number)o).intValue());
						} else if("short".equals(sub)) {
							return new Short(((Number)o).shortValue());
						} else if("byte".equals(sub)) {
							return new Byte(((Number)o).byteValue());
						} else if("long".equals(sub)) {
							return new Long(((Number)o).longValue());
						} else if("float".equals(sub)) {
							return new Float(((Number)o).floatValue());
						} else if("double".equals(sub)) {
							return new Double(((Number)o).doubleValue());
						} else if("char".equals(sub)) {
							return new Character((char)((Number)o).intValue());
						}
					} else {
						throw new WrongExpressionException("Cannot convert to " + sub + ".");
					}
				} else {
					Expression subExpression = new Expression(expression, index + 1, m - 1);
					Object o = subExpression.compute();
					index = m;
					skipSpaces();
					return o;
				}
			} else if(ch == '+') {
				index++;
				return computeOperand();
			} else if(ch == '-') {
				index++;
				Object o = computeOperand();
				if(o instanceof Number) {
					Number n = (Number)o;
					if(n instanceof Long) return new Long(-n.longValue());
					if(n instanceof Float) return new Float(-n.floatValue());
					if(n instanceof Double) return new Double(-n.doubleValue());
					return new Integer(-n.intValue());
				} else if(o instanceof Character) {
					if(o instanceof Character) return new Integer(-((Character)o).charValue());
				} else {
					throw new WrongExpressionException("Cannot compute negative of non-number");
				}
			} else {
				int e = getOperandTokenEnd();
				if(e == index) {
					throw new WrongExpressionException("Operand expected at " + index);
				}
				String t = expression.substring(index, e);
				index = e;
				if(isNameToken(t)) {
					try {
						constant = null;
						Object o = connect() ? referenceResolver.resolveReference(t) : null;						
						if(constant != null) {
							return constant;
						} else if (o != null) {
							return o;
						}
						throw new WrongExpressionException("Cannut resolve name " + t);
					} catch (CoreException exc) {
						CommonCorePlugin.getPluginLog().logError(exc);
					}
				} else {
					try {
						return Integer.parseInt(t);
					} catch (NumberFormatException exc) {
						//ignore - user input
					}
					try {
						if(t.toLowerCase().endsWith("l")) {
							t = t.substring(0, t.length() - 1);
						}
						return Long.parseLong(t);
					} catch (NumberFormatException exc) {
						//ignore - user input
					}
					try {
						if(t.toLowerCase().endsWith("d") || t.toLowerCase().endsWith("f")) {
							t = t.substring(0, t.length() - 1);
						}
						return Double.parseDouble(t);
					} catch (NumberFormatException exc) {
						//ignore - user input
					}
				}
				throw new WrongExpressionException("Cannut resolve value " + t);
			}
			return null;
		}
		
	}

	class ReferenceResolver {
		private boolean isConnected = false;
		private ICompilationUnit u = null;
		private ICompilationUnit u2 = null;
		private IType type = null;

		ReferenceResolver() {}

		private boolean connect() throws CoreException {
			if(isConnected) {
				return type != null;
			}
			isConnected = true;
			if(element == null) {
				return false;
			}
			u = (ICompilationUnit)element.getAncestor(IJavaElement.COMPILATION_UNIT);
			if(u == null) {
				return false;
			}
			u2 = null;
			type = (IType)element.getAncestor(IJavaElement.TYPE);
			if(type == null) {
				if(u != null && element.getParent() instanceof IPackageDeclaration) {
					IType[] ts = u.getTypes();
					if(ts != null && ts.length > 0) {
						type = ts[0];
					} else {
						u2 = u.getWorkingCopy(new NullProgressMonitor());
						type = u2.createType("class A {}", null, false, new NullProgressMonitor());
					}
				}
			}
			return type != null;
		}
		
		public void dispose() {
			if (u2 != null) {
				try {
					u2.discardWorkingCopy();
				} catch (JavaModelException e) {
					
				}
				u2 = null;
			}
			u = null;
			type = null;
		}

		/**
		 * Resolves reference to field constant or class name.
		 * Returns resolved qualified name. Initial value of the field
		 * is stored to be retrieved by getConstant().
		 * @param value
		 * @return
		 * @throws CoreException
		 */
		Object resolveReference(Object value) throws CoreException {
			IImportDeclaration[] is = u.getImports();
			String stringValue = value.toString();
			int lastDot = stringValue.lastIndexOf('.');
			String lastToken = stringValue.substring(lastDot + 1);
			if(lastDot < 0) {
				IField f = (element.getParent() == type) ? type.getField(lastToken) : EclipseJavaUtil.findField(type, lastToken);
				if(f != null && f.exists()) {
					value = f.getDeclaringType().getFullyQualifiedName() + "." + lastToken;
					setFieldInitialValueToConstant(f);
				} else {
					String v = getFullName(type, is, lastToken);
					if(v != null) {
						value = v;
						String typeName = v.substring(0, v.length() - lastToken.length() - 1);
						f = findField(type.getJavaProject(), typeName, lastToken);
						if(f != null) {
							setFieldInitialValueToConstant(f);
						}
					}
				}
				return value;
			}
			String prefix = stringValue.substring(0, lastDot);
			String t = EclipseJavaUtil.resolveType(type, prefix);
			if(t != null) {
				IType q = EclipseJavaUtil.findType(type.getJavaProject(), t);
				if(q != null && q.getField(lastToken).exists()) {
					value = t + "." + lastToken;
					IField f = q.getField(lastToken);
					setFieldInitialValueToConstant(f);
				} else {
					String v = getFullName(type, is, lastToken);
					if(v != null && v.endsWith(stringValue)) {
						value = v;
						String typeName = v.substring(0, v.length() - lastToken.length() - 1);
						IField f = findField(type.getJavaProject(), typeName, lastToken);
						if(f != null) {
							setFieldInitialValueToConstant(f);
						}
					}
				}
			}	
			return value;
		}

	}

	static Set<String> PRIMITIVE_TYPES = new HashSet<String>();
	static {
		PRIMITIVE_TYPES.add("int");
		PRIMITIVE_TYPES.add("short");
		PRIMITIVE_TYPES.add("byte");
		PRIMITIVE_TYPES.add("long");
		PRIMITIVE_TYPES.add("float");
		PRIMITIVE_TYPES.add("double");
		PRIMITIVE_TYPES.add("char");
	}

	private static int findMatchingBrace(String expression, int from, int to) {
		int k = 0;
		for (int i = from; i < to; i++) {
			char c = expression.charAt(i);
			if(c == '(') {
				k++;
			} else if(c == ')') {
				k--;
				if(k == 0) {
					return i + 1;
				}
			}
		}
		return -1;
	}

	private static int findMatchingQuote(String expression, int from, int to) {
		char q = expression.charAt(from);
		for (int i = from + 1; i < to; i++) {
			char c = expression.charAt(i);
			if(c == q) return i + 1;
		}
		return -1;
	}

	private static Object add(Object left, int operation, Object right) {
		if(left == null || right == null) {
			return null;
		} else if(left instanceof String || right instanceof String) {
			if(operation == 0) {
				return left.toString() + right;
			}
		} else if(left instanceof Character) {
			return add(new Integer((int)((Character)left).charValue()), operation, right);
		} else if(right instanceof Character) {
			return add(left, operation, new Integer((int)((Character)right).charValue()));
		} else if(left instanceof Number && right instanceof Number) {
			if(operation == 0) {
				return add((Number)left, (Number)right);
			} else if(operation == 1) {
				return subtract((Number)left, (Number)right);
			}
		}
		return null;
	}

	private static Object add(Number left, Number right) {
		if(left instanceof Double || right instanceof Double) {
			return new Double(left.doubleValue() + right.doubleValue());
		} else if(left instanceof Float || right instanceof Float) {
			return new Float(left.floatValue() + right.floatValue());
		} else if(left instanceof Long || right instanceof Long) {
			return new Long(left.longValue() + right.longValue());
		} else {
			return new Integer(left.intValue() + right.intValue());
		}
	}

	private static Object subtract(Number left, Number right) {
		if(left instanceof Double || right instanceof Double) {
			return new Double(left.doubleValue() - right.doubleValue());
		} else if(left instanceof Float || right instanceof Float) {
			return new Float(left.floatValue() - right.floatValue());
		} else if(left instanceof Long || right instanceof Long) {
			return new Long(left.longValue() - right.longValue());
		} else {
			return new Integer(left.intValue() - right.intValue());
		}
	}

	private static Object multiply(Object left, Object right) {
		if(left == null || right == null) {
			return right;
		} else if(left instanceof Number && right instanceof Number) {
			return multiply((Number)left, (Number)right);
		}		
		return null;
	}

	private static Object multiply(Number left, Number right) {
		if(left instanceof Double || right instanceof Double) {
			return new Double(left.doubleValue() * right.doubleValue());
		} else if(left instanceof Float || right instanceof Float) {
			return new Float(left.floatValue() * right.floatValue());
		} else if(left instanceof Long || right instanceof Long) {
			return new Long(left.longValue() * right.longValue());
		} else {
			return new Integer(left.intValue() * right.intValue());
		}
	}

	private static Object divide(Object left, Object right) throws WrongExpressionException {
		if(left == null || right == null) {
			return right;
		}  else if(left instanceof Number && right instanceof Number) {
			return divide((Number)left, (Number)right);
		}		
		return null;
	}

	private static Object divide(Number left, Number right) throws WrongExpressionException {
		if(isZero((Number)right)) {
			throw new WrongExpressionException("Division by zero " + right);
		}
		if(left instanceof Double || right instanceof Double) {
			return new Double( left.doubleValue() / right.doubleValue());
		} else if(left instanceof Float || right instanceof Float) {
			return new Float(left.floatValue() / right.floatValue());
		} else if(left instanceof Long || right instanceof Long) {
			return new Long(left.longValue() / right.longValue());
		} else {
			return new Integer(left.intValue() / right.intValue());
		}
	}	

	private static boolean isZero(Number n) {
		if(n instanceof Double) {
			return Math.abs(n.doubleValue()) < 1E-14;
		} else if(n instanceof Float) {
			return Math.abs(n.floatValue()) < 1E-7;
		} else {
			return n.intValue() == 0;
		}
	}

	private static boolean isNameToken(String t) {
		if(t.length() == 0) {
			return false;
		}
		if(!Character.isJavaIdentifierStart(t.charAt(0))) {
			return false;
		}
		for (int i = 1; i < t.length(); i++) {
			char ch = t.charAt(i);
			if(!Character.isJavaIdentifierPart(ch) && ch != '.') {
				return false;
			}
		}
		return true;
	}

	private static String getFullName(IType type, IImportDeclaration[] is, String name) throws CoreException {
		for (IImportDeclaration d: is) {
			String n = d.getElementName();
			if(n.equals(name) || n.endsWith("." + name)) {
				return n;
			}
			if(Flags.isStatic(d.getFlags()) && n.endsWith(".*")) {
				String typename = n.substring(0, n.length() - 2);
				IType t = EclipseJavaUtil.findType(type.getJavaProject(), typename);
				if(t != null && t.exists()) {
					IField f = EclipseJavaUtil.findField(t, name);
					if(f != null) {
						return f.getDeclaringType().getFullyQualifiedName() + "." + name;
					}
				}
				
			}
		}
		return null;
	}

	private static IField findField(IJavaProject jp, String typeName, String fieldName) throws CoreException {
		IType t = EclipseJavaUtil.findType(jp, typeName);
		if(t == null && typeName.lastIndexOf('.') > 0) {
			int i = typeName.lastIndexOf('.');
			String innerType = typeName.substring(0, i) + "$" + typeName.substring(i + 1);
			t = EclipseJavaUtil.findType(jp, innerType);
		}
		if(t != null) {
			IField f = t.getField(fieldName);
			if(f != null && f.exists()) {
				return f;
			}
		}
		return null;
	}

}
