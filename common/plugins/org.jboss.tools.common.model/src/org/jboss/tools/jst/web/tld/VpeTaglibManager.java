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
import java.util.Map;


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
	 * Return template taglib prefix using prefix as a key.
	 * @return
	 */
	public String getTemplateTaglibPrefix(String sourceTaglibPrefix);

	/**
	 * This is a way to use templateTaglibs from
	 * org.jboss.tools.vpe.editor.template.VpeTemplateManager;
	 * this is just reference to VpeTemplateManager.templateTaglibs
	 * getter is prohibited here 
	 **/
	public void setTemplateTaglibs(Map<String,String> templateTaglibs);
}