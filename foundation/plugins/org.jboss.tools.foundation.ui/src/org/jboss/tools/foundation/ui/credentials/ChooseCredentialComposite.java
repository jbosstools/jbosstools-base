/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.credentials;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;

public class ChooseCredentialComposite extends Composite {
	
	public static final int STYLE_STANDARD = 1;
	public static final int STYLE_PASSWORD_OVERRIDE = 2;
	
	
	protected ChooseCredentialComponent component;
	
	/**
	 * Draw the credential selection composite allowing all domains
	 */
	public ChooseCredentialComposite(Composite parent) {
		this(parent, null);
	}
	
	/**
	 * Draw the credential selection composite allowing selected domains, 
	 * or, if null, all domains. 
	 * 
	 * @param parent	The parent composite
	 * @param domains	The selected domains, or null to mean all domains
	 */
	public ChooseCredentialComposite(Composite parent, String[] domains) {
		this(parent, domains, null);
	}
	
	/**
	 * 
	 * @param parent
	 * @param domains
	 * @param selectedUsername
	 */
	public ChooseCredentialComposite(Composite parent, String[] domains, String selectedUsername) {
		this(parent, domains, selectedUsername, STYLE_STANDARD);
	}
	
	public ChooseCredentialComposite(Composite parent, String[] domains, String selectedUsername, int type) {
		super(parent, SWT.NONE);
		if( type == STYLE_STANDARD) 
			component = new ChooseCredentialComponent(domains, selectedUsername);
		else if( type == STYLE_PASSWORD_OVERRIDE)
			component = new ChooseCredentialOverridePasswordComponent(domains, selectedUsername);
		setLayout(new GridLayout(3, false));
		component.create(this);
		component.gridLayout(3);
	}

	
	public void addCredentialListener(ICredentialCompositeListener listener) {
		component.addCredentialListener(listener);
	}
	public void removeCredentialListener(ICredentialCompositeListener listener) {
		component.removeCredentialListener(listener);
	}
	
	public ICredentialDomain getDomain() {
		return component.getDomain();
	}
	
	public String getUser() {
		return component.getUser();
	}
	
	public String getPassword() {
		return component.getPassword();
	}
	public void setEnabled (boolean enabled) {
		component.setEnabled(enabled);
	}
	

	
}
