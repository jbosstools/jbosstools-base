/*******************************************************************************
 * Copyright (c) 2007-2013 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.attribute.editor;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ElementChangedEvent;
import org.eclipse.jdt.core.IElementChangedListener;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.templates.configuration.MetaClassTemplateHelper;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

class JavaAdapter {
    private String superClass;
    private String[] interfacees;
	private IJavaProject javaProject;
	private IJavaElement javaElement;
	private IPackageFragmentRoot packageFragmentRoot;
	
	private String className;
	private String packageName;
    
    private static JavaAdapter INSTANCE;
    
    long lastTimeChange = 0;
    
    private JavaAdapter() {
        JavaCore.addElementChangedListener(new IElementChangedListener() {
			public void elementChanged(ElementChangedEvent event) {
				lastTimeChange = System.currentTimeMillis();
			}
        });
    }
    
    public static JavaAdapter getInstance() {
        if (INSTANCE==null) {
            INSTANCE = new JavaAdapter();
        }
        return INSTANCE;
    }
    
    public IPackageFragmentRoot getPackageFragmentRoot(IProject project) {
    	if(project == null) return null;
        try {
            javaProject = (IJavaProject)project.getNature(JavaCore.NATURE_ID);
        } catch (CoreException e) {
        	ModelUIPlugin.getPluginLog().logError("Cannot find Java Project Nature.", e); //$NON-NLS-1$
			return null;
        }
		IResource r = EclipseResourceUtil.getJavaSourceRoot(project);
		if(r == null || !r.exists()) r = project;
		return packageFragmentRoot = javaProject == null ? null : javaProject.getPackageFragmentRoot(r);
	}

	public void init(IProject project, String publicId, String xPath, String name) {
		if (publicId != null) {
			this.superClass = MetaClassTemplateHelper.instance.getSuperClassName(project, publicId, xPath);
			this.interfacees = MetaClassTemplateHelper.instance.getInterfacesName(project, publicId, xPath);
		}
		if (project == null) {
			return;
		}
		packageName = null;
		String fullClassName = null;
		if (name == null) name = ""; //$NON-NLS-1$
		fullClassName = name.replace('.', '/') + ".java"; //$NON-NLS-1$
		if (name.lastIndexOf('.') > 0) {
			className = name.substring(name.lastIndexOf('.') + 1);
			packageName = name.substring(0,name.lastIndexOf('.'));
		} else {
			className = name; 
			packageName = ""; //$NON-NLS-1$
		}
        try {
            javaProject = (IJavaProject)project.getNature(JavaCore.NATURE_ID);
        } catch (CoreException e) {
        	ModelUIPlugin.getPluginLog().logError("Cannot find Java Project Nature.", new Exception()); //$NON-NLS-1$
			return;
        }
        
		packageFragmentRoot = javaProject == null ? null : javaProject.getPackageFragmentRoot(javaProject.getResource());
		
		try {
			if(javaProject == null) {
				javaElement = null;
			} else {
	            javaElement = javaProject.findElement(new Path(fullClassName));
	            if(javaElement == null) {
	            	javaElement = javaProject.findType(name);
	            }
	            if(javaElement == null) {
	            	javaElement = javaProject.findType(name, new NullProgressMonitor());
	            }
			}
        } catch (JavaModelException e) {
        	javaElement = null;
//        	no validation here
//			ModelUIPlugin.log("Cannot find Java Class.", e);
        }
    }	    
    
    public String getSuperClass() {
        if (superClass==null || superClass.length()==0) superClass = "java.lang.Object"; //$NON-NLS-1$
        return superClass;
    }
    public void setSuperClass(String superClass) {
        this.superClass = superClass;
    }
    public String[] getInterfacees() {
        return interfacees;
    }
    public void setInterfacees(String[] interfacees) {
        this.interfacees = interfacees;
    }
    public IJavaElement getJavaElement() {
        return javaElement;
    }
    public void setJavaElement(IJavaElement javaElement) {
        this.javaElement = javaElement;
    }
    public String getClassName() {
        return className;
    }
    public String getPackageName() {
    	return packageName;
    }
    public void setClassName(String className) {
        this.className = className;
    }
    public IJavaProject getJavaProject() {
        return javaProject;
    }
    public void setJavaProject(IJavaProject javaProject) {
        this.javaProject = javaProject;
    }
    public IPackageFragmentRoot getPackageFragmentRoot() {
        return packageFragmentRoot;
    }
    public void setPackageFragmentRoot(IPackageFragmentRoot packageFragmentRoot) {
        this.packageFragmentRoot = packageFragmentRoot;
    }
}
