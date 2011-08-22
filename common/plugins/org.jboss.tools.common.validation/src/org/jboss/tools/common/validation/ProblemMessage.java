/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
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

import org.eclipse.wst.validation.internal.core.Message;

/**
 * @author Alexey Kazakov
 */
public class ProblemMessage extends Message {

	private String message;

	public ProblemMessage(String message, int severity, String[] params, Object targetObject, String groupName) {
		super(null, severity, null, params, targetObject, groupName);
		this.message = message;
	}

	public ProblemMessage(String message, int severity, String[] params) {
		super(null, severity, null, params, null);
		this.message = message;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.core.Message#getText()
	 */
	@Override
	public String getText() {
		if (getParams() != null) {
			return java.text.MessageFormat.format(message, getParams());
		}

		return message;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.core.Message#getText(java.lang.ClassLoader)
	 */
	@Override
	public String getText(ClassLoader classLoader) {
		return getText();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.core.Message#getText(java.util.Locale, java.lang.ClassLoader)
	 */
	@Override
	public String getText(Locale locale, ClassLoader classLoader) {
		return getText();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.validation.internal.core.Message#getText(java.util.Locale)
	 */
	@Override
	public String getText(Locale locale) {
		return getText();
	}
}