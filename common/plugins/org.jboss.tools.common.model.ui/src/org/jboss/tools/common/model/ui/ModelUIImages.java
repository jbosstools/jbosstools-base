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
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.ui.CommonUIImages;

public class ModelUIImages extends CommonUIImages {
	private static String ACTIONS_PATH         = "wizards/"; //$NON-NLS-1$
	
	public static String ACT_CREATE_PROJECT    = ACTIONS_PATH + "new_project.gif"; //$NON-NLS-1$
	public static String ACT_ADOPT_PROJECT     = ACTIONS_PATH + "adopt_project.gif"; //$NON-NLS-1$
	public static String ACT_IMPORT_PROJECT    = ACTIONS_PATH + "import_project.gif"; //$NON-NLS-1$
	
	public static String WIZARD_NEW_PROJECT    = ACTIONS_PATH + "EclipseCreateNewProject.png"; //$NON-NLS-1$
	public static String WIZARD_DEFAULT        = ACTIONS_PATH + "EclipseDefault.png"; //$NON-NLS-1$
	public static String WIZARD_IMPORT_PROJECT = ACTIONS_PATH + "EclipseImport.png"; //$NON-NLS-1$
	public static String WIZARD_MODULES_CONFIG = ACTIONS_PATH + "EclipseModulesConfiguration.gif"; //$NON-NLS-1$
	public static String WIZARD_TAG_TEMPLATE   = ACTIONS_PATH + "TagTemplateWizBan.png"; //$NON-NLS-1$
	public static String WIZARD_IMPORT_TAG_TEMPLATE   = ACTIONS_PATH + "ImportTagTemplateWizBan.png"; //$NON-NLS-1$
	public static String WIZARD_EXPORT_TAG_TEMPLATE   = ACTIONS_PATH + "ExportTagTemplateWizBan.png"; //$NON-NLS-1$
	public static String PROPERTIES_FILE_IMAGE    = ACTIONS_PATH + "PropertiesFileWizBan.png"; //$NON-NLS-1$
	public static String EL_REFERENCE_IMAGE    = ACTIONS_PATH + "ELReferenceWizBan.png"; //$NON-NLS-1$
	
	// JAVA
	public static String JAVA_CLASS 			= "java/class.gif"; //$NON-NLS-1$
	public static String JAVA_INTERFACE 		= "java/interface.gif"; //$NON-NLS-1$
	public static String JAVA_PACKAGE 			= "java/package.gif"; //$NON-NLS-1$
	
	public static String TAGLIB_FILE 			= "editors/taglibs_file.gif"; //$NON-NLS-1$
	
	public static String TAGLIB_ATTRIBUTE 			= "editors/taglibs_attribute.gif"; //$NON-NLS-1$
	
	private static ModelUIImages INSTANCE;
	
	static {
		try {
			INSTANCE = new ModelUIImages(new URL(ModelUIPlugin.getDefault().getBundle().getEntry("/"), "images/xstudio/")); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (MalformedURLException e) {
			// do nothing
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}
	
	public static Image getImage(String key) {
		return getInstance().getOrCreateImage(key);
	}

	public static ImageDescriptor getImageDescriptor(String key) {
		return getInstance().getOrCreateImageDescriptor(key);
	}

	public static void setImageDescriptors(IAction action, String iconName)	{
		action.setImageDescriptor(getImageDescriptor(iconName));
	}
	
	public static ModelUIImages getInstance() {
		return INSTANCE;
	}

	protected ModelUIImages(URL registryUrl, ModelUIImages parent){
		super(registryUrl, parent);
	}
	
	protected ModelUIImages(URL url){
		this(url,null);		
	}

	protected ImageRegistry getImageRegistry() {
		return ModelUIPlugin.getDefault().getImageRegistry();
	}
}
