/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.jboss.tools.ui.bot.ext.SWTBotExt;

/**
 * Returns true when active editor has required title.  
 * 
 * @author Vlado Pakan
 *
 */
public class ActiveEditorHasTitleCondition implements ICondition {

	private String editorTitle;
	private SWTBotExt botExt;
	
	public ActiveEditorHasTitleCondition(SWTBotExt botExt,String shellTitle) {
		super();
		this.editorTitle = shellTitle;
		this.botExt = botExt;
	}

	@Override
	public void init(SWTBot bot) {
		// empty
	}
	
	@Override
	public boolean test() throws Exception {
		return (botExt.activeEditor() != null && editorTitle.equals(botExt.activeEditor().getTitle()));
	}
	
	@Override
	public String getFailureMessage() {
		return "Active editor doesn't have title:\n" + editorTitle 
			+ "\nbut it has title:\n" + (botExt.activeEditor() == null ? "<null>" : botExt.activeEditor().getTitle());
	}
}
