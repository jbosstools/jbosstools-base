/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.foundation.help.ui.internal.search;

import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.help.IHelpResource;
import org.eclipse.help.search.ISearchEngineResult;
import org.jboss.dmr.ModelNode;
import org.jboss.dmr.ModelType;

public class SearchiskoResultBuilder {
	
	static int MAX_DESCRIPTION_LENGTH = 180;

	private SearchiskoResultBuilder(){}
	
	public static ISearchEngineResult create(ModelNode hit) {
		Assert.isNotNull(hit);
		if (!hit.isDefined()) {
			return null;
		}
		return new SearchiskoResult(hit);
	}
	
	
	private static class SearchiskoResult implements ISearchEngineResult {

		private ModelNode hit;
		
		private SearchiskoResult(ModelNode hit) {
			this.hit = hit;
		}

		@Override
		public String getLabel() {
			return getField("title");
		}

		@Override
		public String getDescription() {
			return getField("description", MAX_DESCRIPTION_LENGTH);
		}

		@Override
		public IHelpResource getCategory() {
			return null;
		}

		@Override
		public String getHref() {
			return getField("id");
		}

		@Override
		public float getScore() {
			return hit.get("score").asBigDecimal().floatValue();
		}

		@Override
		public boolean getForceExternalWindow() {
			return true;
		}

		@Override
		public String toAbsoluteHref(String href, boolean frames) {
			return href;
		}
		
		private String getField(String name) {
			ModelNode field = hit.get(name);
			if (ModelType.LIST == field.getType()) {
				List<ModelNode> list = field.asList();
				if (!list.isEmpty()) {
					return list.get(0).asString();
				}
			} else {
				return field.asString();
			}
			return "";
		}
		
		private String getField(String name, int maxLength) {
			return StringUtils.abbreviate(getField(name), maxLength);
		}
		
	}
	
}
