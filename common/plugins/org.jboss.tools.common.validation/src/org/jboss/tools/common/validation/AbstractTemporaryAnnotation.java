/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationAccessExtension;
import org.eclipse.jface.text.source.IAnnotationPresentation;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.eclipse.ui.internal.util.BundleUtility;
import org.eclipse.ui.texteditor.AnnotationPreference;
import org.eclipse.ui.texteditor.AnnotationPreferenceLookup;
import org.eclipse.ui.texteditor.ImageUtilities;

/**
 * @author Alexey Kazakov
 */
public abstract class AbstractTemporaryAnnotation extends Annotation implements IAnnotationPresentation {

    private static final int WARNING_LAYER;
    private static final int ERROR_LAYER;

    private Map<String, Object> fAttributes = new HashMap<String, Object>();

    private int seveirty = WARNING_LAYER;

    static {
        AnnotationPreferenceLookup lookup = EditorsUI.getAnnotationPreferenceLookup();
        WARNING_LAYER = computeLayer("org.eclipse.wst.sse.ui.temp.warning", lookup); //$NON-NLS-1$
        ERROR_LAYER = computeLayer("org.eclipse.wst.sse.ui.temp.error", lookup); //$NON-NLS-1$
    }

    private static int computeLayer(String annotationType, AnnotationPreferenceLookup lookup) {
        Annotation annotation = new Annotation(annotationType, false, null);
        AnnotationPreference preference= lookup.getAnnotationPreference(annotation);
        if (preference != null) {
            return preference.getPresentationLayer() + 1;
        } else {
            return IAnnotationAccessExtension.DEFAULT_LAYER + 1;
        }
    }

	public AbstractTemporaryAnnotation(String type, boolean isPersistent, String text, boolean warning) {
		super(type, isPersistent, text);
		this.seveirty = warning?WARNING_LAYER:ERROR_LAYER;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.IAnnotationPresentation#getLayer()
	 */
	@Override
	public int getLayer() {
        return seveirty;
	}

    protected abstract String getWarningIconPath();

    protected abstract String getErrorIconPath();

	@Override
	public void paint(GC gc, Canvas canvas, Rectangle bounds) {
		String path = seveirty==WARNING_LAYER? getWarningIconPath() : getErrorIconPath();
        URL url = BundleUtility.find(IDEWorkbenchPlugin.IDE_WORKBENCH, path);
        ImageDescriptor descriptor = ImageDescriptor.createFromURL(url);
		Image image = descriptor.createImage(false);
		ImageUtilities.drawImage(image, gc, canvas, bounds, SWT.CENTER, SWT.TOP);
	}

	public Object getAttribute(String key) {
		return fAttributes.get(key);
	}
	
	public void setAttribute(String key, Object value) {
		fAttributes.put(key, value);
	}
}