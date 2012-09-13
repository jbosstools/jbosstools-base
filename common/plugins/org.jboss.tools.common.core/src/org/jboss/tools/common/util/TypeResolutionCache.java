/*******************************************************************************
  * Copyright (c) 2011 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.jboss.tools.common.core.CommonCorePlugin;

public class TypeResolutionCache {	
	private static TypeResolutionCache instance = new TypeResolutionCache();

	public static TypeResolutionCache getInstance() {
		return instance;
	}

	static class Resolved {
		IType type;
		Map<String, String> types = new Hashtable<String, String>();
		List<String> classImports = new ArrayList<String>();
		List<String> packageImports = new ArrayList<String>();
		Resolved(IType type) {
			this.type = type;
			readImports();
		}
		
		void setType(IType type) {
			this.type = type;
			types.clear();

			readImports();
		}
	
		void readImports() {
			ICompilationUnit unit = type.getCompilationUnit();
			if(unit == null) return;
			IImportDeclaration[] ds = null;
			try {
				ds = unit.getImports();
			} catch (JavaModelException e) {
				CommonCorePlugin.getDefault().logError(e);
				ds = new IImportDeclaration[0];
			}

			try {
				IType[] ts = type.getTypes();
				for (IType t: ts) {
					types.put(t.getElementName(), t.getFullyQualifiedName('.'));
				}
			} catch (JavaModelException e) {
				CommonCorePlugin.getDefault().logError(e);
			}			

			IResource r = unit.getResource();
			
			if(r instanceof IFile && r.exists()) {
				List<String> newClassImports = new ArrayList<String>();
				List<String> newPackageImports = new ArrayList<String>();
				//add local package
				newPackageImports.add(type.getPackageFragment().getElementName() + "."); //$NON-NLS-1$
				for (IImportDeclaration d: ds) {
					String q = d.getElementName();
					if(q.endsWith(".*")) { //$NON-NLS-1$
						newPackageImports.add( q = q.substring(0, q.length() - 1));
					} else {
						newClassImports.add(q);
					}
				}
				classImports = newClassImports;
				packageImports = newPackageImports;
			}
		}
		
		public String resolveInImports(String typeName) {
			if(typeName.indexOf(".") >= 0) { //$NON-NLS-1$
				try {
					IType q = EclipseJavaUtil.findType(type.getJavaProject(), typeName);
					if(q != null) {
						types.put(typeName, typeName);
						return typeName;
					}
				} catch (JavaModelException e) {
					CommonCorePlugin.getDefault().logError(e);
				}
				//too difficult
				return null; 
			}
			for (String imp: classImports) {
				if(imp.endsWith("." + typeName)) { //$NON-NLS-1$
					types.put(typeName, imp);
					return imp;
				}
			}
			for (String imp: packageImports) {
				String result = imp + typeName;
				try {
					IType q = EclipseJavaUtil.findType(type.getJavaProject(), result);
					if(q != null) {
						types.put(typeName, result);
						return result;
					}
				} catch (JavaModelException e) {
					CommonCorePlugin.getDefault().logError(e);
				}
			}
			String pr = "java.lang." + typeName; //$NON-NLS-1$
			if(primitive.contains(pr)) {
				types.put(typeName, pr);
				return pr;
			}

			if(type.getTypeParameter(typeName).exists()) {
				types.put(typeName, typeName);
				return typeName;
			}
			return null;
		}
	}

	static Set<String> primitive = new HashSet<String>();
	static {
		primitive.add("void"); //$NON-NLS-1$
		primitive.add("int"); //$NON-NLS-1$
		primitive.add("char"); //$NON-NLS-1$
		primitive.add("boolean"); //$NON-NLS-1$
		primitive.add("long"); //$NON-NLS-1$
		primitive.add("short"); //$NON-NLS-1$
		primitive.add("double"); //$NON-NLS-1$
		primitive.add("float"); //$NON-NLS-1$
		primitive.add("java.lang.Object"); //$NON-NLS-1$
		primitive.add("java.lang.Number"); //$NON-NLS-1$
		primitive.add("java.lang.Integer"); //$NON-NLS-1$
		primitive.add("java.lang.Character"); //$NON-NLS-1$
		primitive.add("java.lang.Boolean"); //$NON-NLS-1$
		primitive.add("java.lang.Long"); //$NON-NLS-1$
		primitive.add("java.lang.Short"); //$NON-NLS-1$
		primitive.add("java.lang.Double"); //$NON-NLS-1$
		primitive.add("java.lang.Float"); //$NON-NLS-1$
		primitive.add("java.lang.String"); //$NON-NLS-1$
		primitive.add("java.lang.StringBuffer"); //$NON-NLS-1$
		primitive.add("java.lang.Class"); //$NON-NLS-1$
		primitive.add("java.lang.Deprecated"); //$NON-NLS-1$
		primitive.add("java.lang.SuppressWarnings"); //$NON-NLS-1$
		primitive.add("java.lang.Throwable"); //$NON-NLS-1$
		primitive.add("java.lang.Exception"); //$NON-NLS-1$
		primitive.add("java.lang.RuntimeException"); //$NON-NLS-1$
		primitive.add("java.lang.Override"); //$NON-NLS-1$
	}
	static final String NULL = ";;;"; //$NON-NLS-1$
	Map<String,Resolved> resolved = new Hashtable<String, Resolved>();

	private TypeResolutionCache() {}
	
	public String resolveType(IType type, String typeName) {
		if(type == null) return null;
		if(type.isBinary() || typeName == null || primitive.contains(typeName)) return typeName;

		String n = getKey(type);
		Resolved r = resolved.get(n);
		if(r == null) {
			r = new Resolved(type);
			resolved.put(n, r);
		} else if(r.type != type) {
			r.setType(type);
		}
		
		String result = r.types.get(typeName);		
		if(result != null) {
			return (result == NULL) ? null : result;
		}

		result = r.resolveInImports(typeName);
		if(result != null) {
			return result;
		}
		
		result = __resolveType(type, typeName);
		
//		System.out.println(typeName + "---" + result);
		
		r.types.put(typeName, result == null ? NULL : result);
		return result;

	}
	
	public void clean() {
		resolved = new Hashtable<String, Resolved>();
		EclipseJavaUtil.typeCache = new Hashtable<String, Map<String,IType>>();
	}

	private String __resolveType(IType type, String typeName) {
		try	{
			String resolvedArray[][] = type.resolveType(typeName);
//			resolvedArray == null for primitive types
			if(resolvedArray == null) return typeName;
			typeName = ""; //$NON-NLS-1$
			for (int i = 0; i < resolvedArray[0].length; i++) 
				typeName += (!"".equals(typeName) ? "." : "") + resolvedArray[0][i];  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return typeName;
		} catch (JavaModelException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		} catch (IllegalArgumentException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		}
		return null;
	}
	
	private String getKey(IType type) {
		String n = type.getFullyQualifiedName();
		IJavaProject jp = type.getJavaProject();
		if(jp == null) return n;
		IProject p = jp.getProject();
		if(p == null || !p.isAccessible()) return n;
		return p.getName() + ":" + n; //$NON-NLS-1$
	}

}