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

import java.util.Arrays;
import java.util.Comparator;

import org.eclipse.core.internal.resources.ICoreConstants;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
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
import org.jboss.tools.common.el.core.GlobalELReferenceList;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.resref.core.ResourceReference;
import org.jboss.tools.common.text.ext.hyperlink.xpl.AbstractBaseHyperlink;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

/**
 * 
 */
@SuppressWarnings("restriction")
abstract public class AbstractHyperlink extends AbstractBaseHyperlink implements IHyperlink {
	public static final String DOLLAR_PREFIX = "${"; //$NON-NLS-1$

    private static final String SUFFIX = "}"; //$NON-NLS-1$

    public static final String SHARP_PREFIX = "#{"; //$NON-NLS-1$
    
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
		} finally {
			smw.dispose();
		}
	}
	
	protected String getBaseLocation() {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			return smw.getBaseLocation();
		} finally {
			smw.dispose();
		}
	}
	
	protected XModel getXModel() {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			return smw.getXModel();
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
		if (wsRelativePath.startsWith("/")) { //$NON-NLS-1$
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

	public static String getBaseLocation(IStructuredModel structuredModel) {
		if (structuredModel == null) {
			return null;
		}
		return structuredModel.getBaseLocation();
	}
	
	public static IFile getFile(IStructuredModel structuredModel) {
		if (structuredModel == null) {
			return null;
		}
		String wsRelativePath = structuredModel.getBaseLocation();
		if (wsRelativePath == null) {
			return null;
		}
		if (wsRelativePath.startsWith("/")) { //$NON-NLS-1$
			IPath path = new Path(wsRelativePath);
			IResource r = ModelPlugin.getWorkspace().getRoot().findMember(
					path);
			if (r != null && r.exists() && r instanceof IFile)
				return (IFile) r;
		}
		String path = Platform.getLocation().append(wsRelativePath)
				.toFile().getAbsolutePath();
		return EclipseResourceUtil.getFile(path);
	}

	abstract protected IRegion doGetHyperlinkRegion(int offset);

	abstract protected void doHyperlink(IRegion region);

	protected IFile getFileFromProject(String fileName) {
		IFile documentFile = getFile();
		if(documentFile == null || !documentFile.isAccessible()) return null;
		
		fileName = findAndReplaceElVariable(fileName);
		
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
			if (name.startsWith("/")) { //$NON-NLS-1$
				member = findFileByAbsolutePath(project, modules[i], name);
			} else {
				member = findFileByRelativePath(project, modules[i],
						currentPath, name);
				if (member == null && name.length() > 0) {
					// in some cases path having no leading "/" is
					// nevertheless absolute
					member = findFileByAbsolutePath(project, modules[i],
							"/" + name); //$NON-NLS-1$
				}
			}
			if (member != null && (member instanceof IFile)) {
				if (((IFile) member).exists())
					return (IFile) member;
			}
		}
		return null;
	}
	
	// partly copied from org.jboss.tools.vpe.editor.util.ElService
	protected String findAndReplaceElVariable(String fileName){
		final IPath workspacePath = Platform.getLocation();

        final ResourceReference[] gResources = GlobalELReferenceList.getInstance().getAllResources(workspacePath);
		String result = fileName;

		ResourceReference[] sortedReferences = sortReferencesByScope(gResources);

		for (ResourceReference rf : sortedReferences) {
			final String dollarEl = DOLLAR_PREFIX + rf.getLocation() + SUFFIX;
			final String sharpEl = SHARP_PREFIX + rf.getLocation() + SUFFIX;

			if (fileName.contains(dollarEl)) {
				result = result.replace(dollarEl, rf.getProperties());
			}
			if (fileName.contains(sharpEl)) {
				result = result.replace(sharpEl, rf.getProperties());
			}
		}
		return result;
	}
	
	// copied from org.jboss.tools.vpe.editor.util.ElService
	private ResourceReference[] sortReferencesByScope(ResourceReference[] references) {
		ResourceReference[] sortedReferences = references.clone();

        Arrays.sort(sortedReferences, new Comparator<ResourceReference>() {
			public int compare(ResourceReference r1, ResourceReference r2) {
				return r1.getScope() - r2.getScope();
			}
        });

		return sortedReferences;
	}
	
	private IFile findFileByRelativePath(IProject project,
			WorkbenchComponent module, IPath basePath, String path) {
		
		if (path == null || path.trim().length() == 0)
			return null;
		
		path = findAndReplaceElVariable(path);
		
		ComponentResource[] resources = module.findResourcesBySourcePath(
				new Path("/"), 0); //$NON-NLS-1$
		IPath projectPath = project.getLocation();
		IFile member = null;

		for (int i = 0; resources != null && i < resources.length; i++) {
			IPath runtimePath = resources[i].getRuntimePath();
			IPath sourcePath = resources[i].getSourcePath();

			// Look in source environment
			IPath webRootPath = projectPath.append(sourcePath);
			IPath relativePath = Utils.getRelativePath(webRootPath,
					basePath);
			IPath filePath = relativePath.append(path);
			member = project.getFolder(sourcePath).getFile(filePath);
			if (member.exists()) {
				return member;
			}

			// Look in runtime environment
			if (runtimePath.segmentCount() >= ICoreConstants.MINIMUM_FOLDER_SEGMENT_LENGTH - 1) {
				webRootPath = projectPath.append(runtimePath);
				relativePath = Utils.getRelativePath(webRootPath, basePath);
				filePath = relativePath.append(path);
				member = project.getFolder(runtimePath).getFile(filePath);
				if (member.exists()) {
					return member;
				}
			}
		}
		return null;
	}

	private IFile findFileByAbsolutePath(IProject project,
		WorkbenchComponent module, String path) {
		ComponentResource[] resources = module.findResourcesBySourcePath(
				new Path("/"), 0); //$NON-NLS-1$
		
		path = findAndReplaceElVariable(path);

		IFile member = null;

		for (int i = 0; resources != null && i < resources.length; i++) {
			IPath runtimePath = resources[i].getRuntimePath();
			IPath sourcePath = resources[i].getSourcePath();

			// Look in source environment
			member = project.getFolder(sourcePath).getFile(path);
			if(member.exists()) {
					return member;
			} 

			// Look in runtime environment
			if (runtimePath.segmentCount() >= ICoreConstants.MINIMUM_FOLDER_SEGMENT_LENGTH - 1) {
				member = project.getFolder(runtimePath).getFile(path);
					if (member.exists()) {
						return member;
				}
			}
		}
		return null;
	}
	
	protected IEditorInput createEditorInput(String fileString) {
		String jarName = fileString.substring(0,fileString.indexOf("!")); //$NON-NLS-1$

        IFile[] fs = ResourcesPlugin.getWorkspace().getRoot().findFilesForLocation(new Path(jarName));
        if(fs == null || fs.length == 0) return null;
        
        IProject p = fs[0].getProject();
        
        IJavaProject jp = EclipseResourceUtil.getJavaProject(p);
        
        if(jp == null) return null;
        
        IPackageFragmentRoot root = jp.getPackageFragmentRoot(fs[0]);

		String entryName = fileString.substring(fileString.indexOf("!")+2,fileString.length()); //$NON-NLS-1$
		JarEntryFile f = new JarEntryFile(entryName);
		f.setParent(root);
        JarEntryEditorInput jarEditorInput = new JarEntryEditorInput(f) {
        	public boolean equals(Object arg) {
        		return this.toString().equals(arg.toString());
        	}
        };
        return jarEditorInput;
	}

}
