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
package org.jboss.tools.common.ui.marker;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;
import org.jboss.tools.common.ui.CommonUIMessages;

/**
 * @author Daniel Azarov
 */
public class AddSuppressWarningsMarkerResolution implements
		IMarkerResolution2 {
	private IJavaElement element;
	private String preferenceKey;
	
	public AddSuppressWarningsMarkerResolution(IJavaElement element, String preferenceKey){
		this.element = element;
		this.preferenceKey = preferenceKey;
	}

	public String getLabel() {
		return CommonUIMessages.CONFIGURE_PROBLEM_SEVERITY;
	}

	public void run(IMarker marker) {
	}

	public String getDescription() {
		return NLS.bind(CommonUIMessages.CONFIGURE_PROBLEM_SEVERITY, element.getElementName());
	}

	public Image getImage() {
		return JavaPlugin.getImageDescriptorRegistry().get(JavaPluginImages.DESC_OBJS_ANNOTATION);
	}

}
