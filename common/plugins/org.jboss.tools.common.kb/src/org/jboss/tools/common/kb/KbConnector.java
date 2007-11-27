/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.kb;

import java.util.Collection;

/**
 * @author eskimo
 */
public interface KbConnector {

	/**
	 * 
	 * @param query
	 * @return
	 * @throws KbException
	 */
	public Collection getProposals(String query) throws KbException;

	/**
	 * 
	 * @param query
	 * @return
	 * @throws KbException
	 */
	public TagDescriptor getTagInformation(String query) throws KbException;

	/**
	 * 
	 * @param resource
	 */
	public boolean registerResource(KbResource resource);

	/**
	 * 
	 * @param resource
	 */
	public void unregisterResource(KbResource resource);
}