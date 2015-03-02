package org.jboss.tools.foundation.help.ui.internal.search;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.help.search.ISearchEngineResult;
import org.eclipse.help.search.ISearchEngineResultCollector;
import org.eclipse.help.search.ISearchScope;
import org.jboss.tools.foundation.help.ui.internal.search.SearchiskoEngine;
import org.jboss.tools.foundation.help.ui.internal.search.SearchiskoEngineScopeFactory;
import org.jboss.tools.foundation.help.ui.internal.search.SearchiskoResultBuilder;
import org.junit.Before;
import org.junit.Test;

public class SearchiskoEngineTest {
	
	private SearchiskoEngine searchEngine;
    private SearchEngineResultCollector collector;
	private ISearchScope scope;
    
	@Before
	public void setUp() {
		searchEngine = new SearchiskoEngine();
		collector = new SearchEngineResultCollector();
		String searchQuery = "http://dcp.jboss.org/v1/rest/search?query={expression}+AND+sys_type%3A(blogpost+article+webpage+solution)&size=100";
		Dictionary<String, String> parameters = new Hashtable<String, String>(Collections.singletonMap("url", searchQuery));
		scope = new SearchiskoEngineScopeFactory().createSearchScope(null, null, parameters);
	}
	
	@Test
	public void testRun() throws Exception {
		searchEngine.run("Server", scope, collector, new NullProgressMonitor());
		assertEquals(100, collector.results.size());
		assertNull(collector.error);
		for (ISearchEngineResult result : collector.results) {
			assertNotNull(result.getLabel());
			assertNull(result.getCategory());
			assertNotNull(result.getHref());
			assertTrue(result.getDescription().length() <= SearchiskoResultBuilder.MAX_DESCRIPTION_LENGTH);
			assertTrue(result.getScore() > 0);
			assertEquals("foo",result.toAbsoluteHref("foo", false));
			assertTrue(result.getForceExternalWindow());
		}
	}
	
	@Test
	public void testRunQueryWithWildCard() throws Exception {
		searchEngine.run("wilpfly", scope, collector, new NullProgressMonitor());
		assertEquals(0, collector.results.size());
		
		
		searchEngine.run("wil?fly", scope, collector, new NullProgressMonitor());
		assertEquals(100, collector.results.size());
		assertNull(collector.error);
		ISearchEngineResult firstResult = collector.results.get(0);
		assertNotNull(firstResult.getLabel());
		assertNotNull(firstResult.getHref());
		assertTrue(firstResult.getDescription().length() <= SearchiskoResultBuilder.MAX_DESCRIPTION_LENGTH);
	}
	
	@Test
	public void testRunNoResult() throws Exception {
		searchEngine.run("ccccccdkkfkfrgibtuhhvibctdftnknukdjbhhdgifen", scope, collector, new NullProgressMonitor());
		assertEquals(0, collector.results.size());
		assertNull(collector.error);
	}
	
	
	@Test
	public void testRunError() throws Exception {
		final String msg = "Ooops, something hit the fan!";
		searchEngine = new SearchiskoEngine() {
			@Override
			protected Collection<ISearchEngineResult> performQuery(String searchQuery, IProgressMonitor monitor)
					throws IOException {
				throw new IOException(msg);
			}
		};
		searchEngine.run("Seam", scope, collector, new NullProgressMonitor());
		assertEquals(0, collector.results.size());
		assertNotNull(collector.error);
		assertEquals(msg, collector.error.getMessage());
	}
	
	private static class SearchEngineResultCollector implements ISearchEngineResultCollector {

		List<ISearchEngineResult> results = new ArrayList<>();
	    IStatus error;
		
		@Override
		public void accept(ISearchEngineResult result) {
			this.results.add(result);
		}

		@Override
		public void accept(ISearchEngineResult[] results) {
			this.results.addAll(Arrays.asList(results));
		}

		@Override
		public void error(IStatus status) {
			this.error = status;
		}
		
	}

}
