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
package org.jboss.tools.common.model.util;

import java.util.*;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class EclipseJavaUtil {

	public static String getMemberTypeAsString(IMember member) {
		if(member instanceof IField) return getMemberTypeAsString((IField)member);
		if(member instanceof IMethod) return getMemberTypeAsString((IMethod)member);
		return null;
	}

	public static String getMemberTypeAsString(IField f) {
		try	{
			String typeName = new String(Signature.toCharArray(f.getTypeSignature().toCharArray()));
			return resolveType(f.getDeclaringType(), typeName);
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		return null;
	}

	public static String getMemberTypeAsString(IMethod m) {
		try	{
			return resolveTypeAsString(m.getDeclaringType(), m.getReturnType());
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		return null;
	}

	public static String resolveTypeAsString(IType type, String typeName) {
		try	{
			typeName = new String(Signature.toCharArray(typeName.toCharArray()));
			return resolveType(type, typeName);
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		return null;
	}
	
	public static String resolveType(IType type, String typeName) {
		try	{
			String resolvedArray[][] = type.resolveType(typeName);
//			resolvedArray == null for primitive types
			if(resolvedArray == null) return typeName;
			typeName = "";
			for (int i = 0; i < resolvedArray[0].length; i++) 
				typeName += (!"".equals(typeName) ? "." : "") + resolvedArray[0][i]; 
			return typeName;
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		return null;
	}
	
	public static IType findType(IJavaProject javaProject, String qualifiedName) throws JavaModelException {
		if(qualifiedName == null || qualifiedName.length() == 0) return null;
		IType type = javaProject.findType(qualifiedName);
		if(type != null) return type;
		int dot = qualifiedName.lastIndexOf('.');
		String packageName = (dot < 0) ? "" : qualifiedName.substring(0, dot);
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
	
	public static boolean isDerivedClass(String type, String superType, IProject project) {
		if(type == null) return false;
		if(superType == null || superType.equals("java.lang.Object")) return true;
		if(type.equals(superType)) return true;
		IType t = EclipseResourceUtil.getValidType(project, type);
		try {
			return t != null && isDerivedClass(t, superType, new HashSet<String>());
		} catch (JavaModelException e) {
			//ignore - we do not care about JavaModelException here.
			return false;
		}
	}

	static boolean isDerivedClass(IType type, String superType, Set<String> checked) throws JavaModelException {
		if(type == null) return false;
		checked.add(type.getFullyQualifiedName());
		String[] is = type.getSuperInterfaceNames();
		if(is != null) for (int i = 0; i < is.length; i++) {
			String f = EclipseJavaUtil.resolveType(type, is[i]);
			if(f != null && f.equals(superType)) return true;
			if(f == null || checked.contains(f)) continue;
			IType t = EclipseResourceUtil.getValidType(type.getJavaProject().getProject(), f);
			if(isDerivedClass(t, superType, checked)) return true;			
		}
		if(type.isInterface()) return false;
		String f = type.getSuperclassName();
		if(f == null || f.length() == 0 || "java.lang.Object".equals(f)) return false;
		if(f.equals(superType)) return true;
		f = EclipseJavaUtil.resolveType(type, f);
		if(f == null || f.length() == 0 || "java.lang.Object".equals(f)) return false;
		if(f.equals(superType)) return true;
		if(checked.contains(f)) return false;
		type = EclipseResourceUtil.getValidType(type.getJavaProject().getProject(), f);
		return isDerivedClass(type, superType, checked);
	}
}
