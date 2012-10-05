/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.ui.wizard.service;

import java.io.ByteArrayInputStream;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.util.FileUtil;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class RegisterServiceUtil {
	static final String META_INF_FOLDER_NAME = "META-INF"; //$NON-NLS-1$
	static final String SERVICES_FOLDER_NAME = "services"; //$NON-NLS-1$

	/**
	 * Returns true if class %typeName% is listed in existing file 
	 * META-INF/services/%serviceType% in a Java source folder of the project.
	 *   
	 * @param project
	 * @param typeName
	 * @param serviceType
	 * @return
	 * @throws CoreException
	 */
	public static boolean isServiceRegistered(IProject project, String typeName, String serviceType) {
		try {
			IContainer f = getServiceFolder(project, false);
			if(f == null || !f.exists()) {
				return false;
			}
			IFile file = f.getFile(new Path(serviceType));
			return file.exists() && contains(FileUtil.readStream(file), typeName);
		} catch (CoreException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		return false;
	}

	/**
	 * Returns true if %content% contains %typeName% separated from other text by whitespaces.
	 * 
	 * @param content
	 * @param typeName
	 * @return
	 */
	private static boolean contains(String content, String typeName) {
		int p = content.indexOf(typeName);
		while(p >= 0) {
			int p0 = p;
			p += typeName.length();
			if((p == content.length() || Character.isWhitespace(content.charAt(p)))
				&& (p0 == 0 || Character.isWhitespace(content.charAt(p0 - 1))) ) {
				//registered already
				return true;
			}
			p = content.indexOf(typeName, p);
		}
		return false;
	}

	/**
	 * Includes class %typeName% into file META-INF/services/%serviceType% 
	 * in a Java source folder of the project, if it is not registered yet.
	 * If the file does not exist, it is created in the first Java source folder.
	 * Existence of a Java source folder is evident by clients of this method: 
	 * (1) Service creation wizard that creates a new Java class in existing Java source;
	 * (2) Service registration context action, that is run on selected IType or ICompilationUnit in existing Java source.	 * 
	 * 
	 * @param project
	 * @param typeName
	 * @param serviceType
	 * @throws CoreException
	 */
	public static void registerService(IProject project, String typeName, String serviceType) throws CoreException {
		IContainer f = getServiceFolder(project, true);
		if(f != null) {
			IFile file = f.getFile(new Path(serviceType));
			if(file.exists()) {
				String content = FileUtil.readStream(file);
				if(contains(content, typeName)) {
					return;
				}
				if(content.length() > 0 && !content.endsWith("\n")) { //$NON-NLS-1$
					content += "\n"; //$NON-NLS-1$
				}
				content += typeName;
				file.setContents(new ByteArrayInputStream(content.getBytes()), true, true, new NullProgressMonitor());
			} else {
				String content = typeName;
				file.create(new ByteArrayInputStream(content.getBytes()), true, new NullProgressMonitor());
			}	
		}
	}

	public static void registerService(IType type, String serviceType) throws CoreException {
		IProject project = type.getJavaProject().getProject();
		String typeName = type.getFullyQualifiedName();
		registerService(project, typeName, serviceType);		
	}

	/**
	 * Returns the first existing 'META-INF/services' folder in a Java source folder,
	 * or newly created 'META-INF/services' folder in an existing Java source folder if 'create' is set to true,
	 * or null.
	 * 
	 * @param project
	 * @param create
	 * @return
	 * @throws CoreException
	 */
	private static IFolder getServiceFolder(IProject project, boolean create) throws CoreException {
		IContainer m = getMetaInf(project, create);
		if(m != null) {
			IFolder ss = m.getFolder(new Path(SERVICES_FOLDER_NAME));
			if(!ss.exists()) {
				if(!create) {
					return null;
				}
				ss.create(true, true, new NullProgressMonitor());
			}
			return ss;
		}
		return null;
	}

	/**
	 * Returns the first existing META-INF folder in a Java source folder, 
	 * or newly created META-INF folder in an existing Java source folder if 'create' is set to true,
	 * or null.
	 *  
	 * @param project
	 * @param create
	 * @return
	 * @throws CoreException
	 */
	private static IContainer getMetaInf(IProject project, boolean create) throws CoreException {
		IResource[] rs = EclipseUtil.getJavaSourceRoots(project);
		if(rs == null || rs.length == 0) {
			return null;
		}
		for (IResource r: rs) {
			IFolder f = ((IContainer)r).getFolder(new Path(META_INF_FOLDER_NAME));
			if(f.exists()) return f;
		}
		if(!create) {
			return null;
		}
		IFolder f = ((IContainer)rs[0]).getFolder(new Path(META_INF_FOLDER_NAME));
		f.create(true, true, new NullProgressMonitor());
		return f;
	}

}
