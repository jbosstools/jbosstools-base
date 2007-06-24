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
package org.jboss.tools.common.text.ext.hyperlink;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.ui.IEditorInput;
import org.eclipse.wst.common.componentcore.internal.ComponentResource;
import org.eclipse.wst.common.componentcore.internal.StructureEdit;
import org.eclipse.wst.common.componentcore.internal.WorkbenchComponent;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.xpl.AbstractBaseHyperlink;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

/**
 * 
 */
abstract public class AbstractHyperlink extends AbstractBaseHyperlink implements IHyperlink {

	public static XModel getXModel(IFile file) {
		if (file == null)
			return null;
		XModelObject fs = EclipseResourceUtil.getObjectByResource(file);
		if (fs != null)
			return fs.getModel();
		IProject project = file.getProject();
		IModelNature nature = EclipseResourceUtil.getModelNature(project);
		return (nature == null) ? null : nature.getModel();
	}

	protected IFile getFile() {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			return smw.getFile();
		} catch (Exception t) {
			// ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	protected XModel getXModel() {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			return smw.getXModel();
		} catch (Exception t) {
			// ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	public static XModel getXModel(IStructuredModel structuredModel) {
		IFile f = getFile(structuredModel);
		if (f != null)
			return getXModel(f);

		if (structuredModel == null)
			return null;
		String wsRelativePath = structuredModel.getBaseLocation();
		if (wsRelativePath == null)
			return null;
		if (wsRelativePath.startsWith("/")) {
			while (true) {
				int i = wsRelativePath.lastIndexOf('/');
				if (i < 0)
					return null;
				wsRelativePath = wsRelativePath.substring(0, i);
				IPath path = new Path(wsRelativePath);
				IResource r = ModelPlugin.getWorkspace().getRoot().findMember(
						path);
				if (r != null && r.exists()) {
					XModelObject o = EclipseResourceUtil
							.createObjectForResource(r);
					if (o != null)
						return o.getModel();
				}
			}
		}
		return null;
	}

	public static IFile getFile(IStructuredModel structuredModel) {
		if (structuredModel == null)
			return null;
		String wsRelativePath = structuredModel.getBaseLocation();
		if (wsRelativePath == null)
			return null;
		try {
			if (wsRelativePath.startsWith("/")) {
				IPath path = new Path(wsRelativePath);
				IResource r = ModelPlugin.getWorkspace().getRoot().findMember(
						path);
				if (r.exists() && r instanceof IFile)
					return (IFile) r;
			}
			String path = Platform.getLocation().append(wsRelativePath)
					.toFile().getAbsolutePath();
			return EclipseResourceUtil.getFile(path);
		} catch (Exception e) {
			//ignore
			return null;
		}
	}

	abstract protected IRegion doGetHyperlinkRegion(int offset);

	abstract protected void doHyperlink(IRegion region);

	protected IFile getFileFromProject(String fileName) {
		IFile documentFile = getFile();

		try {
			IProject project = documentFile.getProject();
			String name = Utils.trimFilePath(fileName);
			IPath currentPath = documentFile.getLocation()
					.removeLastSegments(1);
			IResource member = null;
			StructureEdit se = StructureEdit.getStructureEditForRead(project);
			if (se == null) {
				return null;
			}
			WorkbenchComponent[] modules = se.getWorkbenchModules();
			for (int i = 0; i < modules.length; i++) {
				if (name.startsWith("/")) {
					member = findFileByAbsolutePath(project, modules[i], name);
				} else {
					member = findFileByRelativePath(project, modules[i],
							currentPath, name);
					if (member == null && name.length() > 0) {
						// in some cases path having no leading "/" is
						// nevertheless absolute
						member = findFileByAbsolutePath(project, modules[i],
								"/" + name);
					}
				}
				if (member != null) {
					if (((IFile) member).exists())
						return (IFile) member;
				}
			}
		} catch (Exception x) {
			//ignore
		}
		return null;
	}

	private IFile findFileByRelativePath(IProject project,
			WorkbenchComponent module, IPath basePath, String path) {
		try {
			ComponentResource[] resources = module.findResourcesBySourcePath(
					new Path("/"), 0);
			IPath projectPath = project.getLocation();
			IResource member = null;

			for (int i = 0; resources != null && i < resources.length; i++) {
				IPath runtimePath = resources[i].getRuntimePath();
				IPath sourcePath = resources[i].getSourcePath();

				// Look in source environment
				IPath webRootPath = projectPath.append(sourcePath);
				IPath relativePath = Utils.getRelativePath(webRootPath,
						basePath);
				IPath filePath = relativePath.append(path);
				member = project.getFolder(sourcePath).findMember(filePath);
				if (member != null) {
					if (((IFile) member).exists())
						return (IFile) member;
				}

				// Look in runtime environment
				webRootPath = projectPath.append(runtimePath);
				relativePath = Utils.getRelativePath(webRootPath, basePath);
				filePath = relativePath.append(path);
				member = project.getFolder(runtimePath).findMember(filePath);
				if (member != null) {
					if (((IFile) member).exists())
						return (IFile) member;
				}
			}
		} catch (Exception x) {
			//ignore
		}
		return null;
	}

	private IFile findFileByAbsolutePath(IProject project,
			WorkbenchComponent module, String path) {
		try {
			ComponentResource[] resources = module.findResourcesBySourcePath(
					new Path("/"), 0);
//			IPath projectPath = project.getLocation();
			IResource member = null;

			for (int i = 0; resources != null && i < resources.length; i++) {
				IPath runtimePath = resources[i].getRuntimePath();
				IPath sourcePath = resources[i].getSourcePath();

				// Look in source environment
				member = project.getFolder(sourcePath).findMember(path);
				if (member != null) {
					if (((IFile) member).exists())
						return (IFile) member;
				}

				// Look in runtime environment
				member = project.getFolder(runtimePath).findMember(path);
				if (member != null) {
					if (((IFile) member).exists())
						return (IFile) member;
				}
			}
		} catch (Exception x) {
			//ignore
		}
		return null;
	}
	
	protected IEditorInput createEditorInput(String fileString) {
		String jarName = fileString.substring(0,fileString.indexOf("!"));

        IFile[] fs = ExtensionsPlugin.getDefault().getWorkspace().getRoot().findFilesForLocation(new Path(jarName));
        if(fs == null || fs.length == 0) return null;
        
        IProject p = fs[0].getProject();
        
        IJavaProject jp = EclipseResourceUtil.getJavaProject(p);
        
        if(jp == null) return null;
        
        IPackageFragmentRoot root = jp.getPackageFragmentRoot(fs[0]);

		String entryName = fileString.substring(fileString.indexOf("!")+2,fileString.length());
		JarEntryFile f = new JarEntryFile(entryName);
		f.setParent(root);
        JarEntryEditorInput jarEditorInput = new JarEntryEditorInput(f) {
        	public boolean equals(Object arg) {
        		try {return this.toString().equals(arg.toString());}
        		catch (Exception x) {return false;}
        	}
        };
        return jarEditorInput;
	}

}
