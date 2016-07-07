/*******************************************************************************
 * Copyright (c) 2016 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.ui.test.ext;

import javax.inject.Inject;

import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

public class TestView extends ViewPart {

	@Inject private IPreferencesService preferenceService;
	
	public boolean isValid() {
		return preferenceService != null;
	}

	public void createPartControl(Composite parent) {}
	@Override
	public void setFocus() {}
}
