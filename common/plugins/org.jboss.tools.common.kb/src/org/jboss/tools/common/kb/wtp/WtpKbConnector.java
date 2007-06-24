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
package org.jboss.tools.common.kb.wtp;

import java.util.List;

import org.jboss.tools.common.kb.AttributeDescriptor;
import org.jboss.tools.common.kb.KbConnector;
import org.jboss.tools.common.kb.KbException;
import org.jboss.tools.common.kb.KbProposal;

/**
 * Connector for WTP Content assist processors.
 * @author Igels
 */
public interface WtpKbConnector extends KbConnector {

	/**
	 * 
	 * @param query
	 * @return
	 * @throws KbException
	 */
	public KbProposal getProposal(String query) throws KbException;

	/**
	 * 
	 * @param uri
	 * @return
	 * @throws KbException
	 */
	public List getAllTagNamesFromTldByUri(String uri, String version) throws KbException;

	/**
	 * 
	 * @param query
	 * @return
	 * @throws KbException
	 */
	public AttributeDescriptor getAttributeInformation(String query) throws KbException;
}