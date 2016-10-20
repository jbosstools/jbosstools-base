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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.help.search.ISearchEngine;
import org.eclipse.help.search.ISearchEngineResult;
import org.eclipse.help.search.ISearchEngineResultCollector;
import org.eclipse.help.search.ISearchScope;
import org.jboss.dmr.ModelNode;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.help.ui.internal.JBossHelpActivator;
import org.jboss.tools.foundation.help.ui.internal.search.SearchiskoEngineScopeFactory.Scope;

/**
 * Performs search queries against a <a href="https://github.com/searchisko/searchisko">Searchisko</a> instance.
 * (eg. <a href="http://dcp.jboss.org/v2/rest/">http://dcp.jboss.org/v2/rest/</a>).
 * 
 * @author Fred Bricon
 */
public class SearchiskoEngine implements ISearchEngine {

	@Override
	public void run(String query, ISearchScope scope, ISearchEngineResultCollector collector, IProgressMonitor monitor) {
		try {
			String searchQuery = getSearchUrl(((Scope)scope).getURLTemplate(), query);
			Collection<ISearchEngineResult> results = performQuery(searchQuery, monitor);
			collector.accept(results.toArray(new ISearchEngineResult[results.size()]));
		} catch (Exception e) {
			IStatus error = new Status(IStatus.ERROR, JBossHelpActivator.PLUGIN_ID, e.getLocalizedMessage());
			JBossHelpActivator.getDefault().getLog().log(error);
			collector.error(error);
		}

	}

	protected Collection<ISearchEngineResult> performQuery(String searchQuery, IProgressMonitor monitor)
			throws IOException {
		List<ISearchEngineResult> results = null;
		ByteArrayOutputStream response = new ByteArrayOutputStream(64);
		new URLTransportUtility().download("Searching for help", searchQuery, response , monitor);
		ModelNode searchResult = ModelNode.fromJSONStream(new ByteArrayInputStream(response.toByteArray()));
		if (searchResult.isDefined()) {
			ModelNode hitsWrapper = searchResult.get("hits");
			if (hitsWrapper.isDefined()) {
				ModelNode hitsNode = hitsWrapper.get("hits");
				if (hitsNode.isDefined()) {
					List<ModelNode> hits = hitsNode.asList();
					results = new ArrayList<>(hits.size());
					for (ModelNode hit : hits) {
						if (monitor.isCanceled()) {
							return results;
						}
						ISearchEngineResult result = SearchiskoResultBuilder.create(hit);
						if (result != null) {
							results.add(result);
						}
					}
				}

			}
		}
		return results == null ? Collections.<ISearchEngineResult> emptyList() : results;
	}

	protected String getSearchUrl(String urlTemplate, String query) {
		String eQuery;
		try {
			eQuery = URLEncoder.encode(query, "UTF-8"); //$NON-NLS-1$
		} catch (UnsupportedEncodingException e) {
			eQuery = query;
		}
		return urlTemplate.replace("{expression}", eQuery);
	}

}
