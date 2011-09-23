/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.java;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.util.EclipseJavaUtil;

public class ParametedTypeFactory { 
	// I S J C F D Z
	static HashMap<Character,String> primitives = new HashMap<Character, String>();
	static {
		primitives.put(Signature.C_INT, "Ljava.lang.Integer;");
		primitives.put(Signature.C_SHORT, "Ljava.lang.Short;");
		primitives.put(Signature.C_LONG, "Ljava.lang.Long;");
		primitives.put(Signature.C_CHAR, "Ljava.lang.Character;");
		primitives.put(Signature.C_FLOAT, "Ljava.lang.Float;");
		primitives.put(Signature.C_DOUBLE, "Ljava.lang.Double;");
		primitives.put(Signature.C_BOOLEAN, "Ljava.lang.Boolean;");
		primitives.put(Signature.C_BYTE, "Ljava.lang.Boolean;");
	}
	//unresolved Object signature
	public static String OBJECT = "QObject;"; //$NON-NLS-1$
	Map<String, ParametedType> cache = new HashMap<String, ParametedType>();

	public ParametedType newParametedType(IType type) {
		ParametedType parametedType = new ParametedType();
		if(type != null && !type.isBinary()) {
			ISourceRange r = null;
			try {
				r = type.getNameRange();
			} catch (CoreException e) {
				CommonPlugin.getDefault().logError(e);
			}
			if(r != null) {
				parametedType = new TypeDeclaration(parametedType, type.getResource(), r.getOffset(), r.getLength());
			}
		}
		parametedType.setFactory(this);
		parametedType.setType(type);
		if(type != null) parametedType.setSignature(Signature.C_RESOLVED + type.getFullyQualifiedName() + Signature.C_SEMICOLON);
		String[] ps = null;
		try {
			ps = type.getTypeParameterSignatures();
		} catch (JavaModelException e) {
			CommonPlugin.getDefault().logError(e);
		}
		if(ps != null && ps.length > 0) {
			for (int i = 0; i < ps.length; i++) {
				try {
					ParametedType p = getParametedTypeForParameter(type, ps[i], null);
					if(p != null) parametedType.addParameter(p);
				} catch (JavaModelException e) {
					CommonPlugin.getDefault().logError(e);
				}
			}
		}
		return parametedType;
	}

	public ParametedType getParametedType(IMember context, String typeSignature) throws JavaModelException {
		return getParametedType(context, null, typeSignature);
	}

	public ParametedType getParametedType(IMember context, IParametedType basetype, String typeSignature) throws JavaModelException {
		if(typeSignature == null) return null;
		if(basetype != null) {
			ParametedType param = ((ParametedType)basetype).findParameter(typeSignature);
			if(param != null) return param;
		}
		
		IType contextType = context instanceof IType ? (IType)context : context.getDeclaringType();

		String key = context == null || context.isBinary() || OBJECT.equals(typeSignature) ? typeSignature : contextType.getFullyQualifiedName() + "+" + typeSignature;
		if(cache.containsKey(key)) return cache.get(key);
		ParametedType result = new ParametedType();
		result.setFactory(this);
		result.setSignature(typeSignature);

		typeSignature = typeSignature.substring(result.getArrayPrefix().length());
		
		char c = typeSignature.length() == 0 ? '\0' : typeSignature.charAt(0);
		if(primitives.containsKey(c) && typeSignature.length() == 1) {
			typeSignature = primitives.get(c);
			result.setSignature(result.getArrayPrefix() + typeSignature);
			result.setPrimitive(true);
		} else if(c == Signature.C_EXTENDS) {
			typeSignature = typeSignature.substring(1);
			result.setUpper(true);
		} else if(c == Signature.C_SUPER) {
			typeSignature = typeSignature.substring(1);
			result.setLower(true);
		} else if(c == Signature.C_STAR && typeSignature.length() == 1) {
			result.setUpper(true);
			return result;
		}

		int startToken = typeSignature.indexOf(Signature.C_GENERIC_START);
		if(startToken < 0) {
			String resovedTypeName = EclipseJavaUtil.resolveTypeAsString(contextType, typeSignature);
			if(resovedTypeName == null) return null;
			if(!context.isBinary() || typeSignature.charAt(0) == Signature.C_TYPE_VARIABLE) {
				StringBuffer ns = new StringBuffer();
				ns.append(result.getArrayPrefix());
				if(result.isLower()) ns.append(Signature.C_SUPER);
				if(result.isUpper()) ns.append(Signature.C_EXTENDS);
				ns.append(Signature.C_RESOLVED).append(resovedTypeName).append(Signature.C_SEMICOLON);
				result.setSignature(ns.toString());
			}
			IType type = EclipseJavaUtil.findType(context.getJavaProject(), resovedTypeName);
			if(type != null) {
				result.setType(type);
				cache.put(key, result);
				return result;
			}
			if(context instanceof IMethod) {
				String[] ps = ((IMethod)context).getTypeParameterSignatures();
				for (int i = 0; i < ps.length; i++) {
					ParametedType st = getParametedTypeForParameter(context, ps[i], result);
					if(st != null) {
						if(st.getSignature().indexOf(Signature.C_COLON) >= 0) {
							CommonPlugin.getDefault().logWarning("Wrong signature=" + st.getSignature());
						}
						return st;
					}
				}
			}
			String[] ps = contextType.getTypeParameterSignatures();
			for (int i = 0; i < ps.length; i++) {
				ParametedType st = getParametedTypeForParameter(contextType, ps[i], result);
				if(st != null) return st;
			}
		} else {
			int endToken = typeSignature.lastIndexOf(Signature.C_GENERIC_END);
			if(endToken < startToken) return null;
			String typeName = typeSignature.substring(0, startToken) + typeSignature.substring(endToken + 1);
			String resovedTypeName = EclipseJavaUtil.resolveTypeAsString(contextType, typeName);
			if(resovedTypeName == null) return null;
			IType type = EclipseJavaUtil.findType(context.getJavaProject(), resovedTypeName);
			if(type != null) {
				result.setType(type);
				cache.put(key, result);
				StringBuffer newParams = new StringBuffer();
				String[] paramSignatures = null;
				try {
					paramSignatures = Signature.getTypeArguments(typeSignature);
				} catch (IllegalArgumentException e) {
					CommonPlugin.getDefault().logError(e);
				}
				if(paramSignatures != null) for (String paramSignature: paramSignatures) {
					ParametedType param = getParametedType(context, basetype, paramSignature);
					if(param == null) {
						param = new ParametedType();
						param.setSignature(paramSignature);
					}
					result.addParameter(param);
					newParams.append(param.getSignature());
				}
				if(!context.isBinary()) {
					StringBuffer ns = new StringBuffer();
					ns.append(result.getArrayPrefix());
					if(result.isLower()) ns.append(Signature.C_SUPER);
					if(result.isUpper()) ns.append(Signature.C_EXTENDS);
					ns.append(Signature.C_RESOLVED)
					  .append(resovedTypeName)
					  .append(Signature.C_GENERIC_START)
					  .append(newParams)
					  .append(Signature.C_GENERIC_END)
					  .append(Signature.C_SEMICOLON);
					result.setSignature(ns.toString());
				}
				return result;
			}
		}
		return null;
	}

	public ParametedType getParametedTypeForParameter(IMember context, String typeParameterSignature, ParametedType result) throws JavaModelException {
		IType contextType = context instanceof IType ? (IType)context : context.getDeclaringType();
		String key = context == null ? typeParameterSignature : contextType.getFullyQualifiedName() + "+" + typeParameterSignature;

		String t = Signature.getTypeVariable(typeParameterSignature);
		String[] bounds = Signature.getTypeParameterBounds(typeParameterSignature);
		
		t = Signature.C_RESOLVED + t + Signature.C_SEMICOLON;
		if(result == null || t.equals(result.getSignature())) {
			if(bounds.length > 0 && bounds[0].length() > 0) {
				ParametedType st = getParametedType(contextType, bounds[0]);
				if(st != null) {
					result = new TypeDeclaration(st, context.getResource(), 0, 0);
					result.setUpper(true);
				}
			} else if(result != null) {
				result.setSignature(t);
			}
			if(result == null) {
				result = new ParametedType();
				result.setFactory(this);
				result.setSignature(t);
			}
			result.setVariable(true);
			cache.put(key, result);
			return result;
		}
		return null;
	}

	public void clean() {
		cache.clear();
	}
}