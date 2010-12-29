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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeHierarchy;
import org.eclipse.jdt.core.JavaModelException;

public class EclipseJavaUtil extends org.jboss.tools.common.util.EclipseJavaUtil {

	public static boolean isDerivedClass(String type, String superType, IProject project) {
		if(type == null) return false;
		if(superType == null || superType.equals("java.lang.Object")) return true; //$NON-NLS-1$
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
		if(f == null || f.length() == 0 || "java.lang.Object".equals(f)) return false; //$NON-NLS-1$
		if(f.equals(superType)) return true;
		f = EclipseJavaUtil.resolveType(type, f);
		if(f == null || f.length() == 0 || "java.lang.Object".equals(f)) return false; //$NON-NLS-1$
		if(f.equals(superType)) return true;
		if(checked.contains(f)) return false;
		type = EclipseResourceUtil.getValidType(type.getJavaProject().getProject(), f);
		return isDerivedClass(type, superType, checked);
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
	
}