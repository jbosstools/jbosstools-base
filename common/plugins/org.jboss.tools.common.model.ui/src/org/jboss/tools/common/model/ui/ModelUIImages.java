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
package org.jboss.tools.common.model.ui;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public class ModelUIImages {
//	private URL imageBaseURL = null;
//	private static String PREFIX_ICON_ENABLED  = "";
//	private static String PREFIX_ICON_DISABLED = "d";
//	private static String PREFIX_ICON_HOVER    = "h";
	
	private static String ACTIONS_PATH         = "wizards/";
	
	public static String ACT_CREATE_PROJECT    = ACTIONS_PATH + "new_project.gif";
	public static String ACT_ADOPT_PROJECT     = ACTIONS_PATH + "adopt_project.gif";
	public static String ACT_IMPORT_PROJECT    = ACTIONS_PATH + "import_project.gif";
	
	public static String WIZARD_NEW_PROJECT    = ACTIONS_PATH + "EclipseCreateNewProject.png";
	public static String WIZARD_DEFAULT        = ACTIONS_PATH + "EclipseDefault.png";
	public static String WIZARD_IMPORT_PROJECT = ACTIONS_PATH + "EclipseImport.png";
	public static String WIZARD_MODULES_CONFIG = ACTIONS_PATH + "EclipseModulesConfiguration.gif";
	
	// JAVA
	public static String JAVA_CLASS 			= "java/class.gif";
	public static String JAVA_INTERFACE 		= "java/interface.gif";
	public static String JAVA_PACKAGE 			= "java/package.gif";

	// this blok staye witout changes for compatibility
	private static ModelUIImages INSTANCE;
	
	static {
		try {
			INSTANCE = new ModelUIImages(new URL(ModelUIPlugin.getDefault().getBundle().getEntry("/"), "images/xstudio/"));
		} catch (MalformedURLException e) {
			// do nothing
			ModelUIPlugin.log(e);
		}
	}
	
	public static Image getImage(String key) {
		if(ModelUIPlugin.isDebugEnabled()) {
			ModelUIPlugin.log("Create image for key '"+key+"'.");
		}
		return INSTANCE.createImageDescriptor(key).createImage();
	}

	public static ImageDescriptor getImageDescriptor(String key) {
		if(ModelUIPlugin.isDebugEnabled()) {
			ModelUIPlugin.log("Create image descriptor for key '"+key+"'.");
		}
		return INSTANCE.createImageDescriptor(key);
	}

	public static void setImageDescriptors(IAction action, String iconName)	{
		action.setImageDescriptor(INSTANCE.createImageDescriptor(iconName));
	}
	
	public static ModelUIImages getInstance() {
		return INSTANCE;
	}

	// for reusable purposes
	
	private URL baseUrl;
	private ModelUIImages parentRegistry;
	
	protected ModelUIImages(URL registryUrl, ModelUIImages parent){
		if(ModelUIPlugin.isDebugEnabled()) {
			ModelUIPlugin.log("Create ModelUIImages class.");
			ModelUIPlugin.log("RegistryUrl = " + registryUrl);
			ModelUIPlugin.log("parent = " + (parent==null?"null":parent.getClass().getName()));
		}

		if(registryUrl == null) throw new NullPointerException("Base url for image registry cannot be null.");
		baseUrl = registryUrl;
		parentRegistry = parent;
	}
	
	protected ModelUIImages(URL url){
		this(url,null);		
	}

	public Image getImageByFileName(String key) {
		return createImageDescriptor(key).createImage();
	}

	public ImageDescriptor createImageDescriptor(String key) {
		try {
			return ImageDescriptor.createFromURL(makeIconFileURL(key));
		} catch (MalformedURLException e) {
			if(parentRegistry == null) {
				return ImageDescriptor.getMissingImageDescriptor();
			} else {
				return parentRegistry.createImageDescriptor(key);
			}
			
		}		
	}

	private URL makeIconFileURL(String name) throws MalformedURLException {
		if (name == null) throw new MalformedURLException("Image name cannot be null.");
		return new URL(baseUrl, name);
	}	

}
