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
package org.jboss.tools.foundation.core.credentials;

/**
 * A class to safely extend, to implement ICredentialListener 
 * while avoiding breakages from any api changes, or 
 * to help keep code concise when implementers only
 * care about one event. 
 */
public class CredentialAdapter implements ICredentialListener {

	@Override
	public void domainAdded(ICredentialDomain domain) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void domainRemoved(ICredentialDomain domain) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void credentialAdded(ICredentialDomain domain, String user) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void credentialRemoved(ICredentialDomain domain, String user) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void credentialChanged(ICredentialDomain domain, String user) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void defaultUsernameChanged(ICredentialDomain domain, String user) {
		// TODO Auto-generated method stub
		
	}

}
