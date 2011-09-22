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
import java.util.StringTokenizer;

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
	static HashMap<String,String> primitives = new HashMap<String, String>();
	static {
		primitives.put("I", "Ljava.lang.Integer;");
		primitives.put("S", "Ljava.lang.Short;");
		primitives.put("J", "Ljava.lang.Long;");
		primitives.put("C", "Ljava.lang.Character;");
		primitives.put("F", "Ljava.lang.Float;");
		primitives.put("D", "Ljava.lang.Double;");
		primitives.put("Z", "Ljava.lang.Boolean;");
	}
	//unresolved Object signature
	public static String OBJECT = "QObject;";
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
		if(type != null) parametedType.setSignature("L" + type.getFullyQualifiedName() + ";");
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
		if(typeSignature == null) return null;
		
		IType contextType = context instanceof IType ? (IType)context : context.getDeclaringType();

		String key = context == null || context.isBinary() || OBJECT.equals(typeSignature) ? typeSignature : contextType.getFullyQualifiedName() + "+" + typeSignature;
		if(cache.containsKey(key)) return cache.get(key);
		ParametedType result = new ParametedType();
		result.setFactory(this);
		result.setSignature(typeSignature);

		typeSignature = typeSignature.substring(result.getArrayPrefix().length());
		
		if(primitives.containsKey(typeSignature)) {
			typeSignature = primitives.get(typeSignature);
			result.setSignature(result.getArrayPrefix() + typeSignature);
			result.setPrimitive(true);
		} else if(typeSignature.startsWith("+")) {
			typeSignature = typeSignature.substring(1);
			result.setUpper(true);
		} else if(typeSignature.startsWith("-")) {
			typeSignature = typeSignature.substring(1);
			result.setLower(true);
		} 

		int startToken = typeSignature.indexOf('<');
		if(startToken < 0) {
			String resovedTypeName = EclipseJavaUtil.resolveTypeAsString(contextType, typeSignature);
			if(resovedTypeName == null) return null;
			if(!context.isBinary()) {
				StringBuffer ns = new StringBuffer();
				ns.append(result.getArrayPrefix());
				if(result.isLower()) ns.append('-');
				if(result.isUpper()) ns.append('+');
				ns.append('L').append(resovedTypeName).append(";");
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
						if(st.getSignature().indexOf(':') >= 0) {
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
			int endToken = typeSignature.lastIndexOf('>');
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
					ParametedType param = getParametedType(context, paramSignature);
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
					if(result.isLower()) ns.append('-');
					if(result.isUpper()) ns.append('+');
					ns.append('L').append(resovedTypeName).append('<').append(newParams).append(">;");
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
		
		t = "L" + t + ";";
		if(result == null || t.equals(result.getSignature())) {
			String sts = bounds.length > 0 ? bounds[0] : "";
			if(sts.length() > 0) {
				ParametedType st = getParametedType(contextType, sts);
				if(st != null) {
					result = new TypeDeclaration(st, context.getResource(), 0, 0);
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