/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.el.ui.preferences;

import org.jboss.tools.common.model.ui.preferences.CompanyPreferencesPage;

/**
 * @author Alexey Kazakov
 */
public class ELPreferencesPage extends CompanyPreferencesPage {

	public static final String ID = "org.jboss.tools.el.ui"; //$NON-NLS-1$

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.preferences.CompanyPreferencesPage#getPrefsName()
	 */
	@Override
	protected String getPrefsName() {
		return Messages.EL;
	}
}