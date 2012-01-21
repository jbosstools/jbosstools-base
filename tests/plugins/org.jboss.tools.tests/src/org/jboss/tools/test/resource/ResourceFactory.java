/******************************************************************************* 
 * Copyright (c) 2007-2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.test.resource;

import java.io.IOException;
import java.io.StringBufferInputStream;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.jboss.tools.test.util.ResourcesUtils;

public class ResourceFactory {
	
	static public final IFile createFile(String content) {
		Object proxy = Proxy.newProxyInstance(IFile.class.getClassLoader(), new Class<?> [] {IFile.class}, new FileInvocationHandler(content));
		return (IFile)proxy;
	}
	
	static public final IFile createFile(String content, IPath fullPath) {
		Object proxy = Proxy.newProxyInstance(IFile.class.getClassLoader(), new Class<?> [] {IFile.class}, new FileInvocationHandler(content,fullPath));
		return (IFile)proxy;
	}
	
	static public final IFile createFile(String content, String project, String fileName) throws CoreException, IOException {
		IProject prj = ResourcesUtils.createEclipseProject("Test");
		IFile result = prj.getFile(fileName);
		result.create(new StringBufferInputStream(content), true, null);
		return result;
	}
	
	public static class FileInvocationHandler implements InvocationHandler {

		private String content;
		private IPath fullPath;

		public FileInvocationHandler(String content) {
			super();
			this.content = content;
		}

		public FileInvocationHandler(String content,IPath fullPath) {
			this(content);
			this.fullPath = fullPath;
		}

		public Object invoke(Object proxy, Method method, Object[] args)
				throws Throwable {
			Object result =  null;
			if(method.equals(IFile.class.getMethod("getContents"))) {
				result = new StringBufferInputStream(content);
			} else if(method.equals(IFile.class.getMethod("exists"))) {
				result = Boolean.TRUE;
			} else if(method.equals(IFile.class.getMethod("getFullPath"))){
				result = fullPath;
			} else if(method.equals(IFile.class.getMethod("isAccessible"))){
				return true;
			} 
			return result;
		}
	}
}