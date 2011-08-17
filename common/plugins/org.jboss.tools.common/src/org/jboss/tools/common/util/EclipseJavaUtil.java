/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeHierarchy;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.jboss.tools.common.CommonPlugin;

public class EclipseJavaUtil {

	public static String getMemberTypeAsString(IMember member) {
		if(member instanceof IField) return getMemberTypeAsString((IField)member);
		if(member instanceof IMethod) return getMemberTypeAsString((IMethod)member);
		return null;
	}

	public static String getMemberTypeAsString(IField f) {
		if(f == null) return null;
		try	{
			String typeName = new String(Signature.toCharArray(f.getTypeSignature().toCharArray()));
			return resolveType(f.getDeclaringType(), typeName);
		} catch (JavaModelException e) {
			CommonPlugin.getPluginLog().logError(e);
		}
		return null;
	}

	public static String getMemberTypeAsString(IMethod m) {
		if(m == null) return null;
		try	{
			return resolveTypeAsString(m.getDeclaringType(), m.getReturnType());
		} catch (JavaModelException e) {
			CommonPlugin.getPluginLog().logError(e);
		}
		return null;
	}

	public static String resolveTypeAsString(IType type, String typeName) {
		if(type == null || typeName == null) return null;
		typeName = new String(Signature.toCharArray(typeName.toCharArray()));
		return resolveType(type, typeName);
	}

	static String NULL = ";;;"; //$NON-NLS-1$

	static class Resolved {
		IType type;
		Map<String, String> types = new HashMap<String, String>();
		Resolved(IType type) {
			this.type = type;
		}
		
		void setType(IType type) {
			this.type = type;
			types.clear();
		}
	}
	
	static Map<String,Resolved> resolved = new HashMap<String, Resolved>();
	
	static String getKey(IType type) {
		String n = type.getFullyQualifiedName();
		IJavaProject jp = type.getJavaProject();
		if(jp == null) return n;
		IProject p = jp.getProject();
		if(p == null || !p.isAccessible()) return n;
		return p.getName() + ":" + n; //$NON-NLS-1$
	}
	public static String resolveType(IType type, String typeName) {
		if(type == null) return null;
		if(type.isBinary() || typeName == null) return typeName;
		int i = typeName.indexOf("<");
		if(i >= 0) {
			int j = typeName.lastIndexOf(">");
			if(j >= i) {
//				typeName = typeName.substring(0, i);
			}
		}
		
		String n = getKey(type);
		Resolved r = resolved.get(n);
		if(r == null) {
			r = new Resolved(type);
			resolved.put(n, r);
//			if(resolved.size() % 100 == 0) {
//				System.out.println("-->" + resolved.size() + " " + n); //$NON-NLS-1$ //$NON-NLS-2$
//			}
		}
		if(r.type != type) {
			r.setType(type);
		}
		
		String result = r.types.get(typeName);
		
		if(result != null) {
			return (result == NULL) ? null : result;
		}
		
		result = __resolveType(type, typeName);
		
		String nresult = result == null ? NULL : result;
		
		r.types.put(typeName, nresult);
		
//		System.out.println(n + " " + typeName);
		
		return result;

	}
	
	private static String __resolveType(IType type, String typeName) {
		if(type == null || typeName == null) return null;
		try	{
			String resolvedArray[][] = type.resolveType(typeName);
//			resolvedArray == null for primitive types
			if(resolvedArray == null) return typeName;
			typeName = ""; //$NON-NLS-1$
			for (int i = 0; i < resolvedArray[0].length; i++) 
				typeName += (!"".equals(typeName) ? "." : "") + resolvedArray[0][i];  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return typeName;
		} catch (JavaModelException e) {
			CommonPlugin.getPluginLog().logError(e);
		} catch (IllegalArgumentException ee) {
			System.out.println("Illegal " + typeName);
		}
		return null;
	}
	
	public static IType findType(IJavaProject javaProject, String qualifiedName) throws JavaModelException {
		if(qualifiedName == null || qualifiedName.length() == 0) return null;
		IType type = javaProject.findType(qualifiedName);
		if(type != null) return type;
		int dot = qualifiedName.lastIndexOf('.');
		String packageName = (dot < 0) ? "" : qualifiedName.substring(0, dot); //$NON-NLS-1$
		String shortName = qualifiedName.substring(dot + 1);
		IPackageFragmentRoot[] rs = javaProject.getPackageFragmentRoots();
		for (int i = 0; i < rs.length; i++) {
			IPackageFragment f = rs[i].getPackageFragment(packageName);
			if(f == null || !f.exists()) continue;
			ICompilationUnit[] us = f.getCompilationUnits();
			for (int j = 0; j < us.length; j++) {
				IType t = us[j].getType(shortName);
				if(t != null && t.exists()) return t;
			}
		}
		return null;
	}
	
	public static List<IType> getSupperTypes(IType type) throws JavaModelException {
		ITypeHierarchy typeHierarchy = type.newSupertypeHierarchy(new NullProgressMonitor());
		IType[] superTypes = typeHierarchy == null ? null : typeHierarchy.getAllSupertypes(type);
		if(superTypes == null) {
			return Collections.emptyList();
		}
		List<IType> suppers = new ArrayList<IType>();
		for (int i = 0; i < superTypes.length; i++) {
			suppers.add(superTypes[i]);
		}
		return suppers;
	}
	
	public static IAnnotation findAnnotation(IType sourceType, IAnnotatable member, String qulifiedAnnotationName) throws JavaModelException{
		IAnnotation[] annotations = member.getAnnotations();
		String simpleAnnotationTypeName = qulifiedAnnotationName;
		int lastDot = qulifiedAnnotationName.lastIndexOf('.');
		if(lastDot>-1) {
			simpleAnnotationTypeName = simpleAnnotationTypeName.substring(lastDot + 1);
		}
		for (IAnnotation annotation : annotations) {
			if(qulifiedAnnotationName.equals(annotation.getElementName())) {
				return annotation;
			}
			if(simpleAnnotationTypeName.equals(annotation.getElementName())) {
				String fullAnnotationclassName = EclipseJavaUtil.resolveType(sourceType, simpleAnnotationTypeName);
				if(fullAnnotationclassName!=null) {
					IType annotationType = sourceType.getJavaProject().findType(fullAnnotationclassName);
					if(annotationType!=null && annotationType.getFullyQualifiedName().equals(qulifiedAnnotationName)) {
						return annotation;
					}
				}
			}
		}
		return null;
	}

	/**
	 * Finds field declared in the given type or its super types.
	 * 
	 * @param type
	 * @param name
	 * @return
	 * @throws CoreException
	 */
	public static IField findField(IType type, String name) throws CoreException {
		return findField(type, name, new HashSet<IType>());
	}
	private static IField findField(IType type, String name, Set<IType> processed) throws CoreException {
		if(!type.exists() || processed.contains(type)) {
			return null;
		}
		processed.add(type);
		if(type.getField(name).exists()) {
			return type.getField(name);
		}
		IField f = findField(type, type.getSuperclassName(), name, processed);
		String[] is = type.getSuperInterfaceNames();
		for (int i = 0; f == null && i < is.length; i++) {
			f = findField(type, is[i], name, processed);
		}
		if(f == null) {
			IType d = type.getDeclaringType();
			if(d != null && d != type && d.exists()) {
				f = findField(d, name);
			}
		}
		
		return f;
	}
	private static IField findField(IType context, String typeName, String fieldName, Set<IType> processed) throws CoreException {
		typeName = resolveType(context, typeName);
		if(typeName != null) {
			IType s = findType(context.getJavaProject(), typeName);
			return (s != null) ? findField(s, fieldName, processed) : null;
		}
		return null;
	}

}