/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.editors.dnd;

public interface ITagProposal {
	public static final IAttributeValueLoader EMPTY_ATTRIBUTE_VALUE_LOADER = new IAttributeValueLoader() {
		public void fillTagAttributes(IDropWizardModel model) {
			// do nothing
		}
	};
	public static String EMPTY_PREFIX = "";

	public String getName();

	public String getPrefix();

	public IAttributeValueLoader getAttributesValueLoader();

	public String getDisplayString();

	public String getDetails();

}
