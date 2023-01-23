/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.core.classpath;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.core.Messages;

/**
 * 
 * @author Rob Stryker <rob.stryker@redhat.com>
 *
 */
public abstract class AbstractClasspathContainer implements IClasspathContainer {

	public static final String LIB_FOLDER = "lib"; //$NON-NLS-1$
	public static final String LIB_SOURCE_FOLDER = "libsrc"; //$NON-NLS-1$
	public final static String CLASSPATH_CONTAINER_PREFIX = "org.jboss.ide.eclipse.as.classpath.core"; //$NON-NLS-1$
	
	protected IClasspathEntry[] entries;
	protected IPath path;
	protected String description;
	protected String libFolder;
	protected IJavaProject javaProject;

	protected static ClasspathDecorationsManager decorations;
	static {
		
		decorations = new ClasspathDecorationsManager();
	}
	
	public AbstractClasspathContainer(IPath path, String description,
			String libFolder, IJavaProject project) {
		this.path = path;
		this.description = description;
		this.libFolder = libFolder;
		this.javaProject = project;
	}

	public IClasspathEntry[] getClasspathEntries() {
		if (entries == null) {
			entries = computeEntries();
		}
		return entries;
	}

	public String getDescription() {
		return this.description;
	}

	public int getKind() {
		return IClasspathContainer.K_APPLICATION;
	}

	public IPath getPath() {
		return this.path;
	}

	protected IClasspathEntry[] computeEntries() {
		ArrayList<IClasspathEntry> entries = new ArrayList<IClasspathEntry>();

		String baseDir = getBaseDir();
		if (baseDir == null)
			return new IClasspathEntry[0];

		File libDir = new File(baseDir
				+ "/" + LIB_FOLDER + "/" + getLibFolder());//$NON-NLS-1$ //$NON-NLS-2$
		File libSrcDir = new File(baseDir
				+ "/" + LIB_SOURCE_FOLDER + "/" + getLibFolder());//$NON-NLS-1$ //$NON-NLS-2$

		// Lists every modules in the lib dir
		File[] jars = libDir.listFiles(new FileFilter() {
			public boolean accept(File file) {
				return (file.toString().endsWith(".jar"));//$NON-NLS-1$
			}
		});

		if (jars != null) {
			for (int i = 0; i < jars.length; i++) {
				File jarFile = jars[i];
				String jarFileName = jarFile.getName();
				File jarSrcFile = new File(libSrcDir, jarFileName);

				IPath entryPath = new Path(jarFile.toString());

				IPath sourceAttachementPath = null;
				IPath sourceAttachementRootPath = null;

				final ClasspathDecorations dec 
	            = decorations.getDecorations( getDecorationManagerKey(getPath().toString()), entryPath.toString() );
	        
	        
				IClasspathAttribute[] attrs = {};
				if( dec != null ) {
		            sourceAttachementPath = dec.getSourceAttachmentPath();
		            sourceAttachementRootPath = dec.getSourceAttachmentRootPath();
		            attrs = dec.getExtraAttributes();
		        } else if (jarSrcFile.exists()) {
					sourceAttachementPath = new Path(jarSrcFile.toString());
					sourceAttachementRootPath = new Path("/");//$NON-NLS-1$
				}
		        
				IAccessRule[] access = {};
				IClasspathEntry entry = JavaCore.newLibraryEntry( entryPath, sourceAttachementPath, 
						sourceAttachementRootPath, access, attrs, false );
				entries.add(entry);
			}
		}

		return entries.toArray(new IClasspathEntry[entries.size()]);
	}

	protected String getLibFolder() {
		return this.libFolder;
	}

	protected String getBaseDir() {
		try {
			URL installURL = FileLocator.toFileURL(CommonCorePlugin
					.getDefault().getBundle().getEntry("/")); //$NON-NLS-1$
			return installURL.getFile().toString();
		} catch (IOException ioe) {
			// LOG THE ERROR (one day)
			IStatus status = new Status(IStatus.ERROR, CommonCorePlugin.PLUGIN_ID, 
					Messages.AbstractClasspathContainer_error_loading_container, ioe);
			CommonCorePlugin.getDefault().getLog().log(status);
		}
		return null;
	}
	
	public static String getDecorationManagerKey( String container){
    	return container;
    }
	
	protected static ClasspathDecorationsManager getDecorationsManager() {
        return decorations;
    }

	public void install() {
		entries = computeEntries();
		IJavaProject[] javaProjects = new IJavaProject[] {javaProject};
		final IClasspathContainer[] conts = new IClasspathContainer[] { this };
		try {
			JavaCore.setClasspathContainer(path, javaProjects, conts, null);
		} catch (CoreException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		}
	}
	
	public abstract void refresh();

}
