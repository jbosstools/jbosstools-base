/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.ui.widget.editor;

/**
 * @author eskimo
 *
 */
public interface ITaggedFieldEditor extends IFieldEditor {
	
	/**
	 * 
	 * @return
	 */
	public String[] getTags();
	
	/**
	 * 
	 * @param tags
	 */
	public void setTags(String[] tags);
}
