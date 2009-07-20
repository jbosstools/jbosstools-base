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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.jst.web.tld.ITaglibMapping;
import org.jboss.tools.jst.web.tld.IWebProject;
import org.jboss.tools.jst.web.tld.WebProjectFactory;

/**
 * 
 * @author eskimo
 *
 */
public class DropUtils {

	public static final String HTML40_URI = ""; //$NON-NLS-1$

	/**
	 * Get TLD's content.
	 * @param input
	 * @param uri
	 * @return
	 */
	public static String getTldContent(IEditorInput input, String uri) {
		String tldContent = null;
		XModel xModel = null;
		if(input instanceof IModelObjectEditorInput) {
			xModel = ((IModelObjectEditorInput)input).getXModelObject().getModel();
		} else if(input instanceof IFileEditorInput) {
			IFile f = ((IFileEditorInput)input).getFile();
			XModelObject o = EclipseResourceUtil.getObjectByResource(f);
			if(o != null) xModel = o.getModel();
		}
		if(xModel != null) {
			ITaglibMapping mapping = WebProjectFactory.instance.getWebProject(xModel).getTaglibMapping();
			XModelObject xmo = mapping.getTaglibObject(uri);
			if(xmo != null) {
//				tldLocation = EclipseResourceUtil.getResource(xmo).getFullPath().toString();
				FileAnyImpl fai = (FileAnyImpl)xmo;
				tldContent = fai.getAsText();
			}
		}
		return tldContent;
	}

	public static interface AttributeDescriptorValueProvider {
		public void initContext(Properties context);
		public void setProposal(TagProposal proposal);
		public String getTag();
		public boolean canHaveBody();
		public TagAttributesComposite.AttributeDescriptorValue[] getValues();
	}

	/**
	 * Convert mimeData that contain full file path to IFile instance from project 
	 * @param dropData
	 * @return
	 */
	public static IFile getResourceForMimeData(DropData dropData) {
        // estherbin why in code below create url instance -?
        // URL newUrl = null;
        // URI newUri = null;
        // try {
        // newUri = new URI(dropData.getMimeData());
        // // newUrl = new URL(dropData.getMimeData());
        // } catch (URISyntaxException e) {
        // ModelUIPlugin.getPluginLog().logError(e);
        // }
		boolean isWorkspaceFile = dropData.getMimeData() != null && dropData.getMimeData().startsWith("L/");
        IFile file = isWorkspaceFile ? ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(dropData.getMimeData()).removeFirstSegments(1))
        			: EclipseResourceUtil.getFile(dropData.getMimeData());
        
        if (file == null || !file.exists()) {
            URL newUrl = null;
            try {
                newUrl = new URL(dropData.getMimeData());
                file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(newUrl.getPath()));
                // newUrl = new URL(dropData.getMimeData());
            } catch (MalformedURLException e) {
                ModelUIPlugin.getPluginLog().logError(e);
            }
            file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(newUrl.getPath()));
        }

        return file;
    }	

	/**
	 * Find WEB-ROOT container for given project
	 * @param project
	 * @return
	 */
	public static IContainer getWebRootContainer(IProject project) {
		IContainer container = project; 
		// TODO Eskimo - look how it can be done through WTP EMF model for flexible project
		IModelNature modelNature = EclipseResourceUtil.getModelNature(project);
		if(modelNature == null) return project;
		XModelObject o = modelNature.getModel().getByPath("FileSystems/WEB-ROOT"); //$NON-NLS-1$
		if(o != null) {
			container = (IContainer)EclipseResourceUtil.getResource(o);
		}
		if(container == null) {
			IWebProject p = WebProjectFactory.instance.getWebProject(modelNature.getModel());
			container = ResourcesPlugin.getWorkspace().getRoot().getContainerForLocation(new Path(p.getWebRootLocation()));
		}
		return container;
	}
	
	/**
	 * If it is possible, then converts given {@code path} into URL,
	 * otherwise returns {@code path} unchanged
	 * @return
	 */
	public static String convertPathToUrl(String path) {
		try {
			return new File(path).toURL().toString();
		} catch (MalformedURLException e) {
			return path;
		}
	}
}