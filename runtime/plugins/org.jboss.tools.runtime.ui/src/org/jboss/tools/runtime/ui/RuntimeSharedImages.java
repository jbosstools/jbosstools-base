/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui;

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.foundation.ui.plugin.BaseUISharedImages;

public class RuntimeSharedImages extends BaseUISharedImages {
	public static final String CHECKBOX_ON_KEY = "checkbox_on";//$NON-NLS-1$
	public static final String CHECKBOX_OFF_KEY = "checkbox_off";//$NON-NLS-1$
	public static final String ERROR_KEY = "error_image";//$NON-NLS-1$

	private static final String CHECKBOX_ON_PATH = "/icons/xpl/complete_tsk.gif";//$NON-NLS-1$
	private static final String CHECKBOX_OFF_PATH = "/icons/xpl/incomplete_tsk.gif";//$NON-NLS-1$
	private static final String ERROR_PATH = "/icons/xpl/error_tsk.gif";//$NON-NLS-1$

	private static RuntimeSharedImages shared;
	public static RuntimeSharedImages getDefault() {
		if( shared == null )
			shared = new RuntimeSharedImages();
		return shared;
	}
	
	private RuntimeSharedImages() {
		super(RuntimeUIActivator.getDefault().getBundle());
		addImage(CHECKBOX_ON_KEY, CHECKBOX_ON_PATH);
		addImage(CHECKBOX_OFF_KEY, CHECKBOX_OFF_PATH);
		addImage(ERROR_KEY, ERROR_PATH);
	}

	public static Image getImage(String k) {
		return getDefault().image(k);
	}
}
