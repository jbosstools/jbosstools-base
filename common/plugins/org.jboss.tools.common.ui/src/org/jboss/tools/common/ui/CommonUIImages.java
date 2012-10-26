/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.ui;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.ui.CommonUIPlugin;

public class CommonUIImages {
	private static CommonUIImages INSTANCE;

	static {
		try {
			INSTANCE = new CommonUIImages(new URL(CommonUIPlugin.getDefault().getBundle().getEntry("/"), "icons/")); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (MalformedURLException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
	}

	public static final String JAVA_SERVICE_PROVIDER_IMAGE = "wizard/JavaServiceProviderWizBan.png"; //$NON-NLS-1$
	public static final String NEW_JAVA_SERVICE_PROVIDER_IMAGE = "wizard/NewJavaServiceProviderWizBan.png"; //$NON-NLS-1$
	public static final String WEB_SERVICE_IMAGE = "wizard/WebServiceWizBan.png"; //$NON-NLS-1$

	public final static Image getImage(ImageDescriptor descriptor) {
		return CommonUIPlugin.getImageDescriptorRegistry().get(descriptor);
	}

	public static void setImageDescriptors(IAction action, String iconName)	{
		action.setImageDescriptor(getInstance().getOrCreateImageDescriptor(iconName));
	}

	public static CommonUIImages getInstance() {
		return INSTANCE;
	}

	protected URL baseUrl;
	protected CommonUIImages parentRegistry;

	protected CommonUIImages(URL registryUrl, CommonUIImages parent){
		if(registryUrl == null) throw new IllegalArgumentException(CommonUIMessages.IMAGESBASE_URL_FOR_IMAGE_REGISTRY_CANNOT_BE_NULL);
		baseUrl = registryUrl;
		parentRegistry = parent;
	}
	
	protected CommonUIImages(URL url){
		this(url,null);		
	}

	protected ImageRegistry getImageRegistry() {
		return CommonUIPlugin.getDefault().getImageRegistry();
	}

	public final ImageDescriptor getOrCreateImageDescriptor(String key) {
		ImageDescriptor result = null;
		ImageRegistry registry = getImageRegistry();
		synchronized(registry) {
			result = registry.getDescriptor(key);
		}
		if(result == null) {
			result = createImageDescriptor(key);
			if(result != null) {
				synchronized (registry) {
					registry.remove(key);
					registry.put(key, result);
				}
			}
		}
		return result;
	}

	public final Image getOrCreateImage(String key) {
		getOrCreateImageDescriptor(key);
		ImageRegistry registry = getImageRegistry();
		synchronized(registry) {
			return registry.get(key);
		}
	}

	public final Image getImageByFileName(String key) {
		return getOrCreateImage(key);
	}

	public final ImageDescriptor createImageDescriptor(String key) {
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
		if (name == null) throw new MalformedURLException(CommonUIMessages.IMAGESIMAGE_NAME_CANNOT_BE_NULL);
		return new URL(baseUrl, name);
	}

}
