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

import java.util.Hashtable;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.jboss.tools.common.CommonPlugin;

public class TypeResolutionCache {	
	private static TypeResolutionCache instance = new TypeResolutionCache();

	public static TypeResolutionCache getInstance() {
		return instance;
	}

	static class Resolved {
		IType type;
		Map<String, String> types = new Hashtable<String, String>();
		Resolved(IType type) {
			this.type = type;
		}
		
		void setType(IType type) {
			this.type = type;
			types.clear();
		}
	}

	static String NULL = ";;;"; //$NON-NLS-1$
	Map<String,Resolved> resolved = new Hashtable<String, Resolved>();

	private TypeResolutionCache() {}
	
	public String resolveType(IType type, String typeName) {
		if(type == null) return null;
		if(type.isBinary() || typeName == null) return typeName;
		
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

		result = __resolveType(type, typeName);
		
		r.types.put(typeName, result == null ? NULL : result);
		return result;

	}
	
	public void clean() {
		resolved = new Hashtable<String, Resolved>();
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
			CommonPlugin.getPluginLog().logError(e);
		} catch (IllegalArgumentException e) {
			CommonPlugin.getPluginLog().logError(e);
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
