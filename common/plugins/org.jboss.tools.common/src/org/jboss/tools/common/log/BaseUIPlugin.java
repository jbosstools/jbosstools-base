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
package org.jboss.tools.common.log;

import java.util.HashMap;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.common.Messages;

/**
 * Provides an easy way to log status of events.
 * 
 * NOTE: It is useful to make the static method getPluginLog() which provides
 * the interface IPluginLog for using it in your code in future
 * 
 * @author Sergey Vasilyev
 */
public abstract class BaseUIPlugin extends AbstractUIPlugin implements
		IPluginLog {

	// A Map to save a descriptor for each image
	private HashMap<String, ImageDescriptor> fImageDescRegistry = null;

	public void logError(String message, Throwable t) {
		LogHelper.logError(this, message, t);
	}

	public void logError(String message) {
		LogHelper.logError(this, message);
	}

	public void logError(Throwable t) {
		LogHelper.logError(this, t);
	}

	public void logInfo(String message, Throwable t) {
		LogHelper.logInfo(this, message, t);
	}

	public void logInfo(String message) {
		LogHelper.logInfo(this, message);
	}

	public void logWarning(String message, Throwable t) {
		LogHelper.logWarning(this, message, t);
	}

	public void logWarning(String message) {
		LogHelper.logWarning(this, message);
	}

	public void logWarning(Throwable t) {
		LogHelper.logWarning(this, t);
	}

	public void showError(String message, Throwable t) {
		logError(message, t);
		Shell shell = Display.getDefault().getActiveShell();
		IStatus s = StatusFactory.getInstance(IStatus.ERROR, this.getBundle()
				.getSymbolicName(), message, t);
		ErrorDialog.openError(shell, Messages.BaseUIPlugin_ErrorDialogTitle,
				message, s);
	}

	/**
	 * Retrieves the image associated with resource from the image registry. If
	 * the image cannot be retrieved, attempt to find and load the image at the
	 * location specified in resource.
	 * 
	 * @param resource
	 *            the image to retrieve
	 * @return Image the image associated with resource or null if one could not
	 *         be found
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
	 * Creates an image from the given resource and adds the image to the image
	 * registry.
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
	 * Retrieves the image descriptor associated with resource from the image
	 * descriptor registry. If the image descriptor cannot be retrieved, attempt
	 * to find and load the image descriptor at the location specified in
	 * resource.
	 * 
	 * @param resource
	 *            the image descriptor to retrieve
	 * @return ImageDescriptor the image descriptor assocated with resource or
	 *         the default "missing" image descriptor if one could not be found
	 */
	public ImageDescriptor getImageDescriptorFromRegistry(String resource) {
		ImageDescriptor imageDescriptor = null;
		ImageDescriptor o = getImageDescriptorRegistry().get(resource);
		if (o == null) {
			// create a descriptor
			imageDescriptor = createImageDescriptor(resource);
		} else {
			imageDescriptor = o;
		}
		return imageDescriptor;
	}

	/**
	 * Returns the image descriptor registry for this plugin.
	 * 
	 * @return HashMap - image descriptor registry for this plugin
	 */
	private HashMap<String, ImageDescriptor> getImageDescriptorRegistry() {
		if (fImageDescRegistry == null) {
			fImageDescRegistry = new HashMap<String, ImageDescriptor>();
		}
		return fImageDescRegistry;
	}

	/**
	 * Creates an image descriptor from the given imageFilePath and adds the
	 * image descriptor to the image descriptor registry. If an image descriptor
	 * could not be created, the default "missing" image descriptor is returned
	 * but not added to the image descriptor registry.
	 * 
	 * @param imageFilePath
	 * @return ImageDescriptor image descriptor for imageFilePath or default
	 *         "missing" image descriptor if resource could not be found
	 */
	private ImageDescriptor createImageDescriptor(String imageFilePath) {
		ImageDescriptor imageDescriptor = AbstractUIPlugin
				.imageDescriptorFromPlugin(getId(), imageFilePath);
		if (imageDescriptor != null) {
			getImageDescriptorRegistry().put(imageFilePath, imageDescriptor);
		} else {
			imageDescriptor = ImageDescriptor.getMissingImageDescriptor();
		}

		return imageDescriptor;
	}

	/**
	 * Returns ID of the plug-in.
	 * 
	 * @return
	 */
	public abstract String getId();
}