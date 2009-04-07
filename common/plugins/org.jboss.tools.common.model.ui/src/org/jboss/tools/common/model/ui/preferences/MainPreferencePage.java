/*
 * JBoss, a division of Red Hat
 * Copyright 2006, Red Hat Middleware, LLC, and individual contributors as indicated
 * by the @authors tag. See the copyright.txt in the distribution for a
 * full listing of individual contributors.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA, or see the FSF site: http://www.fsf.org.
 */
package org.jboss.tools.common.model.ui.preferences;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jboss.tools.common.model.ui.ModelUIMessages;

/**
 * Empty preference page for JBoss-IDE.
 * 
 * @author Laurent Etiemble
 * @version $Revision: 8012 $
 */

public class MainPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {
	
	public static final String ID = "org.jboss.tools.common.model.ui.MainPreferencePage"; //$NON-NLS-1$
	
	private Image image;

	/** Default constructor */
	public MainPreferencePage() {
		this.setDescription(ModelUIMessages.MainPreferencePage_Description); 
	}

	/** Description of the Method */
	public void dispose() {
		if (this.image != null) {
			this.image.dispose();
		}
		super.dispose();
	}

	/**
	 * Initialization
	 * 
	 * @param workbench
	 *            Workbench
	 */
	public void init(IWorkbench workbench) {
	}

	/**
	 * Create the content of the preference page
	 * 
	 * @param parent
	 *            Parent
	 * @return The content of the preference page
	 */
	protected Control createContents(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);

		noDefaultAndApplyButton();
		return composite;
	}
}
