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
 * Describes Store.
 * @author eskimo
 */
public interface KbStore {

	/**
	 * 
	 * @param query
	 * @return Collection of proposal from store. 
	 */
	public Collection queryProposal(KbQuery query);

	/**
	 * 
	 * @param query
	 * @return tag descriptor
	 */
	public TagDescriptor queryTagInformation(KbQuery query);

	/**
	 * 
	 * @param query
	 * @return attribute descriptor
	 */
	public AttributeDescriptor queryAttributeInformation(KbQuery query);

	/**
	 * Registers resource in store
	 * @param resource
	 */
	public void registerResource(KbResource resource);

	/**
	 * Unregisters resource
	 * @param resource
	 */
	public void unregisterResource(KbResource resource);
}