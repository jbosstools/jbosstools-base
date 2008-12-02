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
package org.jboss.tools.common.kb;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.osgi.framework.BundleContext;

/**
 * @author eskimo
 */
public class KbPlugin extends BaseUIPlugin {

	public static final String PLUGIN_ID = "org.jboss.tools.common.kb";
	private File location;
	
	// The shared instance
	private static KbPlugin plugin;

	// A Map to save a descriptor for each image
	private HashMap fImageDescRegistry = null;

	public static final String CA_ENUMERATION_IMAGE_PATH = "images/ca/icons_Enumeration.gif";

	public KbPlugin() {
	}

	/**
	 * 
	 * @return
	 */
	public static KbPlugin getDefault() {
		return KbPluginHolder.INSTANCE;
	}

	private boolean isLocationSet = false;

	/**
	 * Return plugin Location.
	 * @return
	 */
	public File getLocation() {
		if(!isLocationSet) {
			try {
				isLocationSet = true;
				location = new File(FileLocator.resolve(KbPlugin.getDefault().getBundle().getEntry("/")).getPath());
			} catch (IOException e) {
				getPluginLog().logError(e);
			}
		}
		return location;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean isDebugEnabled() {
		return getDefault().isDebugging();
	}

	/**
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
    public void start(BundleContext context) throws Exception {
        super.start(context);
        KbConfigurationFactory.getInstance().getPluginConfiguration();
    }

    static class KbPluginHolder {
		static KbPlugin INSTANCE = (KbPlugin)Platform.getPlugin(PLUGIN_ID); 
	}
    
	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}
	
	/**
	 * Creates an image from the given resource and adds the image to the
	 * image registry.
	 * 
	 * @param resource
	 * @return Image
	 */
	private Image createImage(String resource) {
		ImageDescriptor desc = getImageDescriptorFromRegistry(resource);
		Image image = null;

		if (desc != null) {
			image = desc.createImage();
			// dont add the missing image descriptor image to the image
			// registry
			if (!desc.equals(ImageDescriptor.getMissingImageDescriptor())) {
				getImageRegistry().put(resource, image);
			}
		}
		return image;
	}

	/**
	 * Creates an image descriptor from the given imageFilePath and adds the
	 * image descriptor to the image descriptor registry. If an image
	 * descriptor could not be created, the default "missing" image descriptor
	 * is returned but not added to the image descriptor registry.
	 * 
	 * @param imageFilePath
	 * @return ImageDescriptor image descriptor for imageFilePath or default
	 *         "missing" image descriptor if resource could not be found
	 */
	private ImageDescriptor createImageDescriptor(String imageFilePath) {
		ImageDescriptor imageDescriptor = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN_ID, imageFilePath);
		if (imageDescriptor != null) {
			getImageDescriptorRegistry().put(imageFilePath, imageDescriptor);
		}
		else {
			imageDescriptor = ImageDescriptor.getMissingImageDescriptor();
		}

		return imageDescriptor;
	}

	/**
	 * Retrieves the image associated with resource from the image registry.
	 * If the image cannot be retrieved, attempt to find and load the image at
	 * the location specified in resource.
	 * 
	 * @param resource
	 *            the image to retrieve
	 * @return Image the image associated with resource or null if one could
	 *         not be found
	 */
	public Image getImage(String resource) {
		Image image = getImageRegistry().get(resource);
		if (image == null) {
			// create an image
			image = createImage(resource);
		}
		return image;
	}

	/**
	 * Retrieves the image descriptor associated with resource from the image
	 * descriptor registry. If the image descriptor cannot be retrieved,
	 * attempt to find and load the image descriptor at the location specified
	 * in resource.
	 * 
	 * @param resource
	 *            the image descriptor to retrieve
	 * @return ImageDescriptor the image descriptor assocated with resource or
	 *         the default "missing" image descriptor if one could not be
	 *         found
	 */
	public ImageDescriptor getImageDescriptorFromRegistry(String resource) {
		ImageDescriptor imageDescriptor = null;
		Object o = getImageDescriptorRegistry().get(resource);
		if (o == null) {
			// create a descriptor
			imageDescriptor = createImageDescriptor(resource);
		}
		else {
			imageDescriptor = (ImageDescriptor) o;
		}
		return imageDescriptor;
	}

	/**
	 * Returns the image descriptor registry for this plugin.
	 * 
	 * @return HashMap - image descriptor registry for this plugin
	 */
	private HashMap getImageDescriptorRegistry() {
		if (fImageDescRegistry == null) {
			fImageDescRegistry = new HashMap();
		}
		return fImageDescRegistry;
	}
}