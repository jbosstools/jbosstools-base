/*******************************************************************************
 * Copyright (c) 2016-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.reddeer.preferences;

import org.eclipse.reddeer.common.logging.Logger;
import org.eclipse.reddeer.core.reference.ReferencedComposite;
import org.eclipse.reddeer.jface.preference.PreferencePage;
import org.eclipse.reddeer.swt.impl.button.RadioButton;

/**
 * Reddeer model of JBoss Tools > Source Lookup preference page
 * 
 * @author jniederm
 */
public class SourceLookupPreferencePage extends PreferencePage  {
	
	protected final static Logger log = Logger.getLogger(SourceLookupPreferencePage.class);
	
	public SourceLookupPreferencePage(ReferencedComposite composite) {
		super(composite, "JBoss Tools", "Source Lookup");
	}
	
	public void setSourceAttachment(SourceLookupPreferencePage.SourceAttachmentEnum option) {
		new RadioButton(referencedComposite, option.getText()).click();
		log.info("Attach sources '" + option.getText() + "' selected.");
	}
	
	public enum SourceAttachmentEnum {
		ALWAYS("Always"),
		NEVER("Never"),
		PROMPT("Prompt");
		
		private String text;
		
		SourceAttachmentEnum(String text) {
			this.text = text;
		}
		
		public String getText() {
			return this.text;
		}
	}
}
