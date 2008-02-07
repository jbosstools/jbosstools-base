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
package org.jboss.tools.jst.web.tld;

import java.util.List;
import org.w3c.dom.Node;


/**
 * @author Igels
 */
public interface VpeTaglibManager {

	/**
	 * Add Taglib Listener to manager
	 * @param listener
	 */
	public void addTaglibListener(VpeTaglibListener listener);

	/**
	 * Remove Taglib Listener from manager
	 * @param listener
	 */
	public void removeTaglibListener(VpeTaglibListener listener);

	/**
	 * Return List of TaglibData(s) where keys are prefixes of TLD.
	 * @return
	 */
	public List<TaglibData> getTagLibs();
	
	/**
	 * Sets node in scope of which we should show content assistent
	 * @param node
	 */
	public void setReferenceNode(Node node);
}