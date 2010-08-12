 /*******************************************************************************
  * Copyright (c) 2007-2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.types;

import static org.junit.Assert.fail;

/**
 * Provides functionality to locate appropriate view type
 * @author jpeterka
 *
 */
public enum ViewType {
	PACKAGE_EXPLORER, PROJECT_EXPLORER, WELCOME, DATA_SOURCE_EXPLORER,
	SERVERS,WEB_PROJECTS,PROBLEMS,DEBUG,GUVNOR_REPOSITORIES,PROPERTIES,
	GUVNOR_RESOURCE_HISTORY,JBOSS_TOOLS_PALETTE;
	
	
	public String getGroupLabel() {
		String viewLabel = "";	
		switch (this) {
			case PROJECT_EXPLORER: viewLabel =  IDELabel.ViewGroup.GENERAL; break;
			case PACKAGE_EXPLORER: viewLabel = IDELabel.ViewGroup.JAVA; break;
			case DATA_SOURCE_EXPLORER: viewLabel = IDELabel.ViewGroup.DATA_MANAGEMENT; break;
			case SERVERS: viewLabel = IDELabel.ViewGroup.SERVER; break;
			case WEB_PROJECTS: viewLabel = IDELabel.ViewGroup.JBOSS_TOOLS_WEB; break;
			case PROBLEMS: viewLabel = IDELabel.ViewGroup.GENERAL; break;
			case DEBUG: viewLabel = IDELabel.ViewGroup.DEBUG; break;
			case GUVNOR_REPOSITORIES: viewLabel = IDELabel.ViewGroup.GUVNOR; break;
			case PROPERTIES: viewLabel = IDELabel.ViewGroup.GENERAL; break;
			case GUVNOR_RESOURCE_HISTORY: viewLabel = IDELabel.ViewGroup.GUVNOR; break;
			case JBOSS_TOOLS_PALETTE: viewLabel = IDELabel.ViewGroup.JBOSS_TOOLS_WEB; break;
			default: fail("Unknown View Type");
		}
		return viewLabel;	
	}
	
	public String getViewLabel() {
		String viewLabel = "";	
		switch (this) {
			case PROJECT_EXPLORER: viewLabel =  IDELabel.View.PROJECT_EXPLORER; break;
			case PACKAGE_EXPLORER: viewLabel = IDELabel.View.PACKAGE_EXPLORER; break;
			case DATA_SOURCE_EXPLORER: viewLabel = IDELabel.View.DATA_SOURCE_EXPLORER; break;
			case SERVERS: viewLabel = IDELabel.View.SERVERS; break;
			case WEB_PROJECTS: viewLabel = IDELabel.View.WEB_PROJECTS; break;
			case PROBLEMS: viewLabel = IDELabel.View.PROBLEMS; break;
			case DEBUG: viewLabel = IDELabel.View.DEBUG; break;
			case GUVNOR_REPOSITORIES: viewLabel = IDELabel.View.GUVNOR_REPOSITORIES; break;
			case PROPERTIES: viewLabel = IDELabel.View.PROPERTIES; break;
			case GUVNOR_RESOURCE_HISTORY: viewLabel = IDELabel.View.GUVNOR_RESOURCE_HISTORY; break;
			case JBOSS_TOOLS_PALETTE: viewLabel = IDELabel.View.JBOSS_TOOLS_PALETTE; break;
			default: fail("Unknown View Type");
		}
		return viewLabel;
	}
}
