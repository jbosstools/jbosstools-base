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
package org.jboss.tools.common.model.ui.navigator;

import java.util.*;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.resource.CompositeImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.markers.XMarkerManager;
import org.jboss.tools.common.model.ui.ModelUIImages;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class LabelDecoratorImpl implements ILabelDecorator {
	public static Image emptyImage = ModelUIImages.getImage("empty_co.gif"); //$NON-NLS-1$
	public static Image errorImage = ModelUIImages.getImage("error_co.gif"); //$NON-NLS-1$
	public static Image warningImage = ModelUIImages.getImage("warning_co.gif"); //$NON-NLS-1$

	List<ILabelProviderListener> listeners = new ArrayList<ILabelProviderListener>();
	static Map<Image,Image> errorImages = new HashMap<Image,Image>();
	static Map<Image,Image> warningImages = new HashMap<Image,Image>();
	
	public LabelDecoratorImpl() {}
	
	public static ILabelProvider decorateLabelProvider(ILabelProvider provider) {
		ILabelDecorator decorator = new LabelDecoratorImpl();
		return new DecoratingLabelProviderExt(provider, decorator);
	}

	static Set<String> missingImages = new HashSet<String>();

	public Image decorateImage(Image image, Object element) {
		int severity = getErrorState(element);
		if(image == null && element instanceof XModelObject && severity > 0) {
			String entity = ((XModelObject)element).getModelEntity().getName();
			if(!missingImages.contains(entity)) {
				missingImages.add(entity);
				ModelUIPlugin.getDefault().logWarning("Problem in " + LabelDecoratorImpl.class.getName() + ": Cannot find icon for entity " + entity); //$NON-NLS-1$ //$NON-NLS-2$
			}
		} else if(severity == IMarker.SEVERITY_ERROR) {
			return getErrorImage(image);
		} else if(severity == IMarker.SEVERITY_WARNING) {
			return getWarningImage(image);
		}
		return image;
	}
	
	private Image getErrorImage(Image image) {
		Image i = (Image)errorImages.get(image);
		if(i == null || i.isDisposed()) {
			ErrorImageDescriptor d = new ErrorImageDescriptor(image, IMarker.SEVERITY_ERROR);
			i = d.createImage();
			registerImage(i);
			errorImages.put(image, i);			
		}
		return i;
	}

	private Image getWarningImage(Image image) {
		Image i = (Image)warningImages.get(image);
		if(i == null || i.isDisposed()) {
			ErrorImageDescriptor d = new ErrorImageDescriptor(image, IMarker.SEVERITY_WARNING);
			i = d.createImage();
			registerImage(i);
			warningImages.put(image, i);			
		}
		return i;
	}

	private void registerImage(Image i) {
		ImageRegistry registry = ModelUIPlugin.getDefault().getImageRegistry();
		String key = Double.toString(Math.random()); //We retrieve created images by maps, let use unique random key for registry.
		synchronized(registry) {
			registry.remove(key); //Just in case, to be on the safe side.
			registry.put(key, i);
		}
	}

	int getErrorState(Object element) {
		if(!(element instanceof XModelObject)) return 0;
		return XMarkerManager.getInstance().getErrorState((XModelObject)element);
	}


	public String decorateText(String text, Object element) {
		return text;
	}

	public void addListener(ILabelProviderListener listener) {
		listeners.add(listener);
	}

	public void dispose() {}

	public boolean isLabelProperty(Object element, String property) {
		return true;
	}

	public void removeListener(ILabelProviderListener listener) {
		listeners.remove(listener);		
	}

}

class ErrorImageDescriptor extends CompositeImageDescriptor {
	Image image;
	int severity = 0;

	public ErrorImageDescriptor(Image image, int severity) {
		this.image = image;
		this.severity = severity;
	}

	protected void drawCompositeImage(int width, int height) {
		ImageData bg= image.getImageData();
		drawImage(bg, 0, 0);
		drawBottomLeft();
	}

	protected Point getSize() {
		Rectangle r = image.getBounds();
		return new Point(r.width, r.height);
	}

	private void drawBottomLeft() {
		Point size= getSize();
		int x= 0;
		if (severity == IMarker.SEVERITY_ERROR && LabelDecoratorImpl.errorImage != null) {
			ImageData data= LabelDecoratorImpl.errorImage.getImageData();
				///getImageData(JavaPluginImages.DESC_OVR_ERROR);
			drawImage(data, x, size.y - data.height);
			x+= data.width;
		}
		if (severity == IMarker.SEVERITY_WARNING && LabelDecoratorImpl.warningImage != null) {
			ImageData data= LabelDecoratorImpl.warningImage.getImageData();
			drawImage(data, x, size.y - data.height);
			x+= data.width;
		}

	}		

}

class DecoratingLabelProviderExt extends DecoratingLabelProvider implements IColorProvider {
	IColorProvider colorProvider;
	
	public DecoratingLabelProviderExt(ILabelProvider provider, ILabelDecorator decorator) {
		super(provider, decorator);
		colorProvider = (provider instanceof IColorProvider) ? (IColorProvider)provider : null;
	}

	public Color getForeground(Object element) {
		return (colorProvider != null) ? colorProvider.getForeground(element) : null;
	}

	public Color getBackground(Object element) {
		return null;
	}
	
}