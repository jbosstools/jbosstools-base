/*******************************************************************************
 * Copyright (c) 2009 Obeo
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Mariot Chauvin <mariot.chauvin@obeo.fr> - initial API and implementation
 *******************************************************************************/

package org.jboss.tools.ui.bot.ext.zest;

import java.util.List;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.matchers.WidgetMatcherFactory;
import org.eclipse.zest.core.widgets.Graph;

/**
 * SWTBot extension for Zest Graph support. May be deprecated,removed when 
 * Zest support is officially added into official SWTBot
 * @author jpeterka
 *
 */
public class SWTZestBot extends SWTWorkbenchBot {
	

	/**
	 * Create SWTBotZestGraph
	 * @param index index of zest graph. Usually zero.
	 * @return the SWTbot zest graph instance 
	 */
	public SWTBotZestGraph getZestGraph(int index) {
		List<? extends Graph> graphs = getFinder().findControls(WidgetMatcherFactory.widgetOfType(Graph.class));		
		SWTBotZestGraph graph = new SWTBotZestGraph(graphs.get(0));
		return graph;
	}
	
}
