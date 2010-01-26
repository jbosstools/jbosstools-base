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
package org.jboss.tools.common.model.ui.wizards;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaConventions;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;

import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author au
 */

public class NewTypeWizardAdapter {
	// common data
	private IJavaProject javaProject;
	private IProject project;
	private boolean rawData = Boolean.TRUE.booleanValue();
	private boolean createMain = Boolean.TRUE.booleanValue();
	private boolean createConstructors = Boolean.TRUE.booleanValue();
	private boolean createInherited = Boolean.TRUE.booleanValue();
	private boolean canBeModified = Boolean.TRUE.booleanValue();
	private IStatus classNameStatus;
	private IStatus packageNameStatus;
	
	// raw data
//	private String rawInterfaceName;
	private HashSet<String> rawInterfaces = new HashSet<String>();
	
	// adapted data
//	private IType superClassType;
	private String superClassName;
//	private IType interfaceType;
//	private String interfaceName;
	private String className;
//	private String classArgs;
	private String packageName;
	private IPackageFragmentRoot packageFragmentRoot;
	private IPackageFragment packageFragment;
	private ArrayList<String> interfaces = new ArrayList<String>();
	

	private NewTypeWizardAdapter() {}
	
	// Interface for us

	public NewTypeWizardAdapter(IProject project) {
		if(project != null) try {
			this.javaProject = (IJavaProject)project.getNature(JavaCore.NATURE_ID);
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		this.project = project;
	}
	
	public void setRawClassName(String rawClassName) {
		this.rawData = Boolean.TRUE.booleanValue();
		this.className = rawClassName;
	}

	public void setRawSuperClassName(String rawSuperClassName) {
		this.rawData = Boolean.TRUE.booleanValue();
		this.superClassName = rawSuperClassName;
	}

	public void setRawPackageName(String rawPackageName) {
		this.rawData = Boolean.TRUE.booleanValue();
		this.packageName = rawPackageName;
	}
	
	public void addRawInterfaceName(String rawInterfaceName) {
		this.rawData = Boolean.TRUE.booleanValue();
		this.rawInterfaces.add(rawInterfaceName);
	}
	
	public void setCanBeModified(boolean canBeModified) {
		this.canBeModified = canBeModified;
	}

	public void setCreateConstructors(boolean createConstructors) {
		this.createConstructors = createConstructors;
	}

	public void setCreateInherited(boolean createInherited) {
		this.createInherited = createInherited;
	}

	public void setCreateMain(boolean createMain) {
		this.createMain = createMain;
	}
	
	// doAdapted
	
	private void doAdapted() {
		//			source folder name, package name, class name
		int loc = className.indexOf(":"); //$NON-NLS-1$
		if (loc != -1) {
			if (loc < className.length()) {
//				classArgs = className.substring(loc + 1, className.length());
				className = className.substring(0, loc);
			}
			if (loc > 0)
				className = className.substring(0, loc);
			else if (loc == 0)
				className = ""; //$NON-NLS-1$
		}
		classNameStatus = JavaConventions.validateJavaTypeName(className);
		
		loc = className.lastIndexOf('.');
		if (loc != -1) {
			packageName = className.substring(0, loc);
			className = className.substring(loc + 1);
			packageNameStatus = JavaConventions.validatePackageName(packageName);
			classNameStatus = JavaConventions.validateJavaTypeName(className);
		}
		if (javaProject == null)
			return;
		try {
			if (packageFragmentRoot == null) {
				IPackageFragmentRoot srcEntryDft = null;
				IPackageFragmentRoot[] roots = javaProject.getPackageFragmentRoots();
				for (int i = 0; i < roots.length; i++) {
					if (roots[i].getKind() == IPackageFragmentRoot.K_SOURCE) {
						srcEntryDft = roots[i];
						break;
					}
				}
				if (srcEntryDft != null)
					packageFragmentRoot = srcEntryDft;
				else {
					packageFragmentRoot = javaProject.getPackageFragmentRoot(javaProject.getResource());
				}
				if (packageFragment == null
						&& packageFragmentRoot != null
						&& packageName != null
						&& packageName.length() > 0) {
					IFolder packageFolder = project.getFolder(packageName);
					packageFragment = packageFragmentRoot
					.getPackageFragment(packageFolder
							.getProjectRelativePath().toOSString());
				}
			}
			//			superclass and interface
			if (this.rawInterfaces != null && this.rawInterfaces.size()>0) {
				Iterator i = this.rawInterfaces.iterator();
				this.interfaces.clear();
				while (i.hasNext()) {
					String _interface = (String)i.next();
					// check interface name
					this.interfaces.add(_interface);
				}
			}
			if (this.superClassName !=null && this.superClassName.length()>0) {
				// check super class
			}

			rawData = Boolean.FALSE.booleanValue();
		} catch (JavaModelException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

/*
	private IType findTypeForName(String typeName) throws JavaModelException {
		if (typeName == null || typeName.length() == 0)
			return null;
		IType type = null;
		String fileName = typeName.replace('.', '/') + ".java"; //$NON-NLS-1$
		IJavaElement element = javaProject.findElement(new Path(fileName));
		if (element == null)
			return null;
		if (element instanceof IClassFile) {
			type = ((IClassFile) element).getType();
		} else if (element instanceof ICompilationUnit) {
			IType[] types = ((ICompilationUnit) element).getTypes();
			type = types[0];
		}
		return type;
	}
*/	
	
	// Interface for NewClassWizardPage

	public IPackageFragmentRoot getPackageFragmentRoot() {
		if (rawData) doAdapted();
		return this.packageFragmentRoot;
	}
	
	public IPackageFragment getPackageFragment() {
		if (rawData) doAdapted();
		return this.packageFragment;
	}
	
	public IType getEnclosingType() {
		if (rawData) doAdapted();
		return null;
	}
	
	public boolean getEnclosingTypeSelection() {
		if (rawData) doAdapted();
		return Boolean.FALSE.booleanValue();
	}
	
	public String getTypeName() {
		if (rawData) doAdapted();
		return this.className;
	}
	
	public String getSuperClass() {
		if (rawData) doAdapted();
		return this.superClassName;
	}
	
	public List getSuperInterfaces() {
		if (rawData) doAdapted();
		return this.interfaces;
	}

	public boolean isCreateMain() {
		if (rawData) doAdapted();
		return this.createMain;
	} 
	
	public boolean isCreateConstructors() {
		if (rawData) doAdapted();
		return this.createConstructors;
	} 
	
	public boolean isCreateInherited() {
		if (rawData) doAdapted();
		return this.createInherited;
	} 
	
	public boolean isCanBeModified() {
		if (rawData) doAdapted();
		return this.canBeModified;
	}
	
	public IStatus getClassNameStatus() {
		return classNameStatus;
	}
	
	public IStatus getPackageNameStatus() {
		return packageNameStatus;
	}
}
