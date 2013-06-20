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
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.web.WebUtils;

/**
 * 
 * @author eskimo
 *
 */
public class DropUtils {

	public static final String HTML40_URI = ""; //$NON-NLS-1$

	public static interface AttributeDescriptorValueProvider {
		public void initContext(Properties context);
		public void setProposal(ITagProposal proposal);
		public void setProposal(ITagProposal proposal, boolean useDeclaredLibsOnly);
		public String getTag();
		public boolean canHaveBody();
		public TagAttributesComposite.AttributeDescriptorValue[] getValues();
		public String getPrefix(String uri, String defaultPrefix);
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
		IContainer[] containers = WebUtils.getWebRootFolders(project, true);
		IContainer container = project;
		if(containers.length>0) {
			container = containers[0];
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