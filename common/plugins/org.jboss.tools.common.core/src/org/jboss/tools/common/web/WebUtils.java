/*******************************************************************************
 * Copyright (c) 2011 - 2015 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.web;

import java.io.File;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
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
import org.jboss.tools.common.core.Messages;

public class WebUtils {

	public static IPath[] getWebInfPaths(IProject project) {
		IContainer[] cs = getWebRootFolders(project, true);
		List<IPath> ps = new ArrayList<IPath>();
		for (IContainer c: cs) {
			IFolder f = c.getFolder(new Path("/WEB-INF")); //$NON-NLS-1$
			if(f.exists()) {
				ps.add(f.getFullPath());
			}
		}
		return ps.toArray(new IPath[ps.size()]);
	}

	public static IPath[] getWebContentPaths(IProject project) {		
		IContainer[] cs = getWebRootFolders(project, true);
		IPath[] ps = new IPath[cs.length];
		for (int i = 0; i < cs.length; i++) {
			ps[i] = cs[i].getFullPath();
		}
		return ps;
	}

	public static IPath getFirstWebContentPath(IProject project) {
		IContainer[] cs = getWebRootFolders(project, true);
		for (IContainer c: cs) {
			return c.getFullPath();
		}
		return null;
	}

	/**
	 * Returns the webroot folder which contain the given resource or null of the resource does not belong to any webroot.
	 * @param resource
	 * @return
	 */
	public static IContainer getWebRootFolder(IResource resource) {
		IProject project = resource.getProject();
		if(project!=null) {
			IContainer[] cs = getWebRootFolders(project, false);
			IPath fullPath = resource.getFullPath();
			for (IContainer c: cs) {
				if(c.getFullPath().isPrefixOf(fullPath)) {
					return c;
				}
			}
		}
		return null;
	}

	/**
	 * Returns the webroot folders for a given project.
	 * 
	 * @param project
	 * @param ignoreDerived
	 * @return IContainer[] array filled with a Roots or an empty array
	 */
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
						if(container.exists() && (!ignoreDerived || !container.isDerived(IResource.CHECK_ANCESTORS))) {
							String path = "/" + container.getProjectRelativePath().toString();
							if(defaultPath!=null && path.equals(defaultPath.toString())) {
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
		} else {
			IFolder www = getCordovaWebRootFolder(project);
			if(www!=null && (!ignoreDerived || !www.isDerived(IResource.CHECK_ANCESTORS))) {
				return new IContainer[] {www};
			}
		}
		return EMPTY_ARRAY;
	}

	private static final String THYM_NATURE_ID = "org.eclipse.thym.core.HybridAppNature";
	private static final String AEROGEAR_NATURE_ID = "org.jboss.tools.aerogear.hybrid.core.HybridAppNature";
	private static final String CONFIG_XML = "config.xml";

	/**
	 * Returns "www" folder of the project if this folder exists and the project is a cordova project.
	 * The project is recognized as a cordova project if it has corresponding nature or config.xml file. 
	 * @param project
	 * @return
	 */
	private static IFolder getCordovaWebRootFolder(IProject project) {
		IFolder webroot = null;
		IResource www = project.findMember("www");
		if(www !=null && www.getType() == IResource.FOLDER) {
			webroot = (IFolder)www;
			try {
				if(project.hasNature(THYM_NATURE_ID) || project.hasNature(AEROGEAR_NATURE_ID)) {
					IResource configXml = project.findMember(CONFIG_XML);
					if(configXml ==null || configXml.getType() != IResource.FILE) {
						configXml = webroot.findMember(CONFIG_XML);
						if(configXml ==null || configXml.getType() != IResource.FILE) {
							webroot = null;
						}
					}
				}
			} catch (CoreException e) {
				CommonCorePlugin.getDefault().logError(e);
			}
		}
		return webroot;
	}

	/**
	 * Finds a page resource in the project. If project is not a WTP one, the method uses 
	 * context resource as a base to search the valid resource for a given path.
	 * 
	 * For example, we have a project that has the following structure:
	 * 
	 * aProject 
	 *   -- /www
	 *      ----/css
	 *          ----/some.css
	 *      ----/pages
	 *          ----index.html
	 *      
	 *  Given such a project that has WTP nature in its set up, the following call:
	 *    
	 *    WebUtils.findResource(</www/index.html IFile object>, "/css/some.css");
	 *  
	 *  will use WTP Roots to search a resource for some.css file.
	 *  
	 *  The same call for such a project that has no WTP nature set up will use '/www/index.html' file
	 *  as a base to search for required CSS Stylesheet. So, it will search in 
	 *  - /www/pages/css/ - there is no some.css here, so the method will continue to search in parent folder
	 *  - /www/css/       - there is 'some.css' file in this folder, so it will be returned as a result
	 *  
	 * @param context - Context resource (might be a current page or a folder resource)
	 * @param filePath - Path to a file to search
	 * @return IResource for a given path in a project or null if resource cannot be found
	 */
	public static IResource findResource (IResource context, String filePath) {
		if (filePath == null)
			throw new IllegalArgumentException(MessageFormat.format(
					Messages.WebUtil_NullArgument, "filePath"));

		if (context == null)
			throw new IllegalArgumentException(MessageFormat.format(
					Messages.WebUtil_NullArgument, "context"));

		IProject project = context.getProject();
		
		// Check if the full absolute file path is provided 
		IPath tmpPath = new Path(filePath);
		if (filePath.startsWith("/") && project.getLocation().isPrefixOf(tmpPath)) {
			IFile file = project.getFile(tmpPath.removeFirstSegments(project.getLocation().segmentCount()));
			if (file != null && file.exists()) {
				return file;
			}
		}
		
		IPath[] webContentPaths = WebUtils.getWebContentPaths(project);
		if (filePath.startsWith("/")) { // Absolute Path
			if (webContentPaths.length > 0) { // WTP Project
				for(IPath webContentPath : webContentPaths) {
					IPath container = webContentPath.segmentCount() > 1 ? 
							webContentPath.removeFirstSegments(1) : project.getFullPath();
					IFile file = project.getFile(container.append(filePath));
					if(file.exists())
						return file;
				}
			} else { // Non-WTP Project
				IContainer parent = context instanceof IContainer ? (IContainer)context : context.getParent();
				IPath path = new Path(filePath).makeRelative();
				while (parent != null) {
					IFile file = parent.getFile(path);
					if (file.exists())
						return file;
					parent = parent.getParent();
				}
			}
		} else {
			if (webContentPaths.length > 0) {
				IContainer contextRoot = getWebRootFolder(context);
				if (contextRoot == null)
					return null;
				IContainer parent = context instanceof IContainer ? (IContainer)context : context.getParent();

				File contextFile = parent.getLocation().toFile();
				String contextRelatedFileName = null;
				try {
					contextRelatedFileName = new File(contextFile.toString() + File.separator + filePath).getCanonicalPath();
				} catch (IOException e) {
					return null;
				}
				
				IPath contextRelatedPath = new Path(contextRelatedFileName);
				IPath webRootRelatedPath = contextRelatedPath.removeFirstSegments(contextRoot.getLocation().segmentCount());
				
				for (IPath webContentPath : webContentPaths) {
					IFile file = project.getFile(webContentPath.removeFirstSegments(1).append(webRootRelatedPath));
					if (file.exists())
						return file;
				}
			} else {
				IFile file = project.getFile(context.getProjectRelativePath()
						.removeLastSegments(1).append(filePath));
				if (file.exists())
					return file;
			}
		}
		
		return null;
	}

	/**
	 * Returns the path of the resource.
	 * If "context" is not null and belongs to the same project as "resource" then this method returns the path of "resource" relative to "context".
	 * For example:
	 * 		context  = "<project1>/web/html/index.html"
	 * 		resource = "<project1>/web/img/pic.gif"
	 * 		getWebPath(context, resource, true) will return "../img/pic.gif";
	 * Otherwise the path of the resource relative to the web root folder is returned. If the resource doesn't belong to any web root then the project relative path is returned.
	 * For example: "/img/pic.gif".
	 * 
	 * @param context
	 * @param resource
	 * @return
	 */
	public static String getWebPath(IFile context, IFile resource) {
		String path;
		if(context==null || !context.getProject().equals(resource.getProject())) {
			IContainer root = getWebRootFolder(resource);
			if(root==null) {
				path = resource.getProjectRelativePath().toString();
			} else {
				path = resource.getFullPath().removeFirstSegments(root.getFullPath().segmentCount()).toString();
			}
			if(!path.startsWith("/")) {
				return "/" + path;
			}
		} else {
			path = resource.getFullPath().makeRelativeTo(context.getParent().getFullPath()).toString();
		}
		return path;
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