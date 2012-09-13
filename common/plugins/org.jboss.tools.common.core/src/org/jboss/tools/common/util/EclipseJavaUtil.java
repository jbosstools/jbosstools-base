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
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeHierarchy;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.jboss.tools.common.core.CommonCorePlugin;

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
			CommonCorePlugin.getPluginLog().logError(e);
		}
		return null;
	}

	public static String getMemberTypeAsString(IMethod m) {
		if(m == null) return null;
		try	{
			return resolveTypeAsString(m.getDeclaringType(), m.getReturnType());
		} catch (JavaModelException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		}
		return null;
	}

	public static String resolveTypeAsString(IType type, String typeName) {
		if(type == null || typeName == null) return null;
		typeName = new String(Signature.toCharArray(typeName.toCharArray()));
		int i = typeName.indexOf(Signature.C_GENERIC_START);
		if(i > 0) typeName = typeName.substring(0, i);
		return resolveType(type, typeName);
	}

	public static String resolveType(IType type, String typeName) {
		return TypeResolutionCache.getInstance().resolveType(type, typeName);
	}

	static Map<String, Map<String, IType>> typeCache = new Hashtable<String, Map<String,IType>>();
	
	public static IType findType(IJavaProject javaProject, String qualifiedName) throws JavaModelException {
		if(qualifiedName == null || qualifiedName.length() == 0) return null;
		Map<String, IType> cache = typeCache.get(javaProject.getElementName());
		if(cache == null) {
			cache = new Hashtable<String, IType>();
			typeCache.put(javaProject.getElementName(), cache);
		} else {
			IType type = cache.get(qualifiedName);
			if(type != null) return type;
		}
		IType type = javaProject.findType(qualifiedName);
		if(type != null) return register(cache, qualifiedName, type);
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
				if(t != null && t.exists()) return register(cache, qualifiedName, t);;
			}
		}
		return null;
	}

	private static IType register(Map<String, IType> cache, String qualifiedName, IType type) {
		cache.put(qualifiedName, type);
		return type;
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

	/**
	 * Returns true if the given annotation has the given full name
	 * @param annotation
	 * @param fullName
	 * @return
	 * @throws JavaModelException
	 */
	public static boolean checkAnnotationByFulltName(IAnnotation annotation, String fullName) throws JavaModelException {
		if(annotation.getElementName().equals(fullName)) {
			return true;
		}
		boolean result = true;
		IType sourceType = null;
		IJavaElement parent = annotation.getParent();
		if(parent instanceof IMember) {
			if(parent instanceof IType) {
				sourceType = (IType)parent;
			} else {
				sourceType = ((IMember)parent).getDeclaringType();
			}
			String fullAnnotationName = EclipseJavaUtil.resolveType(sourceType, annotation.getElementName());
			if(fullAnnotationName!=null) {
				IType annotationType = sourceType.getJavaProject().findType(fullAnnotationName);
				result = annotationType!=null && annotationType.getFullyQualifiedName().equals(fullName);
			} else {
				result = false;
			}
		}
		return result;
	}

	/**
	 * Returns annotation by the short name declared for the given java member and its parents.
	 * Returns null if no annotation found.
	 * @param member
	 * @param name
	 * @param checkParents
	 * @return
	 * @throws JavaModelException
	 */
	public static Set<IAnnotation> findAnnotationsByShortName(IJavaElement element, String name, boolean checkParents) throws JavaModelException {
		return findAnnotationsByShortName(null, element, name, checkParents);
	}

	private static Set<IAnnotation> findAnnotationsByShortName(Set<IAnnotation> result, IJavaElement element, String name, boolean checkParents) throws JavaModelException {
		if(element instanceof IAnnotatable) {
			IAnnotation[] annotations = ((IAnnotatable)element).getAnnotations();
			for (IAnnotation annotation : annotations) {
				String aName = annotation.getElementName();
				int i = aName.lastIndexOf('.');
				if(i>-1) {
					aName = aName.substring(i+1);
				}
				if(aName.equals(name)) {
					if(result==null) {
						result = new HashSet<IAnnotation>();
					}
					result.add(annotation);
					break;
				}
			}
		}
		if(checkParents) {
			IJavaElement parent = element.getParent();
			if(parent instanceof IAnnotatable) {
				return findAnnotationsByShortName(result, parent, name, true);
			}
		}
		return result;
	}
}