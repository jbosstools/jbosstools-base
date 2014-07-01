/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.web;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.wst.common.componentcore.ComponentCore;
import org.eclipse.wst.common.componentcore.internal.util.IModuleConstants;
import org.eclipse.wst.common.componentcore.resources.ITaggedVirtualResource;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;
import org.eclipse.wst.common.componentcore.resources.IVirtualFolder;
import org.eclipse.wst.common.project.facet.core.IFacetedProject;
import org.eclipse.wst.common.project.facet.core.IProjectFacet;
import org.eclipse.wst.common.project.facet.core.ProjectFacetsManager;
import org.jboss.tools.common.core.CommonCorePlugin;

public class WebUtils {

	public static IPath[] getWebInfPaths(IProject project) {		
		IVirtualComponent component = ComponentCore.createComponent(project);	
		if(component != null && component.getRootFolder() != null) {
			List<IPath> ps = new ArrayList<IPath>();
			IContainer[] cs = getWebRootFolders(project, true);
			for (IContainer c: cs) {
				if(c.exists()) {
					IFolder f = c.getFolder(new Path("/WEB-INF")); //$NON-NLS-1$
					if(f.exists()) {
						ps.add(f.getFullPath());
					}
				}
			}
			return ps.toArray(new IPath[ps.size()]);
		}
		return new IPath[0];
	}

	public static IPath[] getWebContentPaths(IProject project) {		
		IVirtualComponent component = ComponentCore.createComponent(project);	
		if(component != null && component.getRootFolder() != null) {

			List<IPath> ps = new ArrayList<IPath>();
			IContainer[] cs = getWebRootFolders(project, true);
			for (IContainer c: cs) {
				if(c.exists()) {
					ps.add(c.getFullPath());
				}
			}
			return ps.toArray(new IPath[ps.size()]);
		}
		return new IPath[0];
	}

	public static IPath getFirstWebContentPath(IProject project) {
		IPath modulePath = null;
		IVirtualComponent vc = ComponentCore.createComponent(project);
		if (vc == null || vc.getRootFolder() == null)
			return null;
		IContainer[] cs = getWebRootFolders(project, true);
		for (IContainer c: cs) {
			if(c.exists()) {
				return c.getFullPath();
			}
		}
		return modulePath;
	}

	/**
	 * Returns the webroot folder which contain the given resource or null of the resource does not belong to any webroot.
	 * @param resource
	 * @return
	 */
	public static IContainer getWebRootFolder(IResource resource) {
		IProject project = resource.getProject();
		if(project!=null) {
			IVirtualComponent vc = ComponentCore.createComponent(project);
			if (vc == null || vc.getRootFolder() == null)
				return null;
			IContainer[] cs = getWebRootFolders(project, false);
			IPath fullPath = resource.getFullPath();
			for (IContainer c: cs) {
				if(c.exists() && c.getFullPath().isPrefixOf(fullPath)) {
					return c;
				}
			}
		}
		return null;
	}

	public static IContainer[] getWebRootFolders(IProject project, boolean ignoreDerived) {
		IFacetedProject facetedProject = null;
		try {
			facetedProject = ProjectFacetsManager.create(project);
		} catch (CoreException e) {
			CommonCorePlugin.getDefault().logError(e);
		}
		
		IProjectFacet DYNAMIC_WEB_FACET = ProjectFacetsManager.getProjectFacet(IModuleConstants.JST_WEB_MODULE);
		if(facetedProject!=null && facetedProject.getProjectFacetVersion(DYNAMIC_WEB_FACET)!=null) {
			IVirtualComponent component = ComponentCore.createComponent(project);
			if(component!=null) {
				IVirtualFolder webRootVirtFolder = component.getRootFolder().getFolder(new Path("/")); //$NON-NLS-1$

				IPath defaultPath = getDefaultDeploymentDescriptorFolder(webRootVirtFolder);
				IContainer defaultResource = null;

				IContainer[] folders = webRootVirtFolder.getUnderlyingFolders();
				if(folders.length > 1){
					ArrayList<IContainer> containers = new ArrayList<IContainer>();
					for(IContainer container : folders){
						if(!ignoreDerived || !container.isDerived(IResource.CHECK_ANCESTORS)) {
							String path = "/" + container.getProjectRelativePath().toString();
							if(path.equals(defaultPath.toString())) {
								defaultResource = container;
							} else {
								containers.add(container);
							}
						}
					}
					if(defaultResource!=null) {
						containers.add(0, defaultResource); // Put default root folder to the first position of the list
					}
					return containers.toArray(new IContainer[containers.size()]);
				} else {
					return folders;
				}
			}
		}
		return EMPTY_ARRAY;
	}


	private static final IContainer[] EMPTY_ARRAY = new IContainer[0];
	public static final String DD_FOLDER_TAG = org.eclipse.wst.common.componentcore.internal.WorkbenchComponent.DEFAULT_ROOT_SOURCE_TAG;

	/**
	 * Returns all the web root folders of the project.
	 * If the project is not a web project then the method will return an empty array.
	 * All the derived resources or resources belonged to derived containers will be eliminated.
	 * If some folder is set as default web root source folder (available since WTP 3.3.1) then this folder will be places in the very beginning of the result array.
	 * @param project
	 * @return
	 */
	public static IPath getDefaultDeploymentDescriptorFolder(IVirtualFolder folder) {
    	IPath returnValue = null;
    	if (folder instanceof ITaggedVirtualResource){
    		returnValue = ((ITaggedVirtualResource)folder).getFirstTaggedResource(DD_FOLDER_TAG);
    	}
    	return returnValue;
	}

}
