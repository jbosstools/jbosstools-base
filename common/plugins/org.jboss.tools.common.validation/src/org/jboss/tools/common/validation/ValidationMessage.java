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

import java.util.Locale;

import org.eclipse.core.resources.IResource;
import org.eclipse.wst.validation.internal.core.Message;
import org.jboss.tools.common.CommonPlugin;

/**
 * @author Alexey Kazakov
 */
public class ValidationMessage extends Message {

	private String message;

	public ValidationMessage(int severity,	String message, IResource target) {
		super(CommonPlugin.PLUGIN_ID, severity, message, null, target);
		this.message = message;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.core.Message#getText(java.util.Locale, java.lang.ClassLoader)
	 */
	@Override
	public java.lang.String getText(Locale locale, ClassLoader classLoader) {
		return message;
	}
}