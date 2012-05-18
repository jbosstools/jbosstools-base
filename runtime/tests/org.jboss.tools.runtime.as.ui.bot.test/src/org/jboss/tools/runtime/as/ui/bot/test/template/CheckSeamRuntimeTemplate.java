package org.jboss.tools.runtime.as.ui.bot.test.template;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SeamPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.matcher.RuntimeMatcher;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.junit.After;
import org.junit.Test;

public abstract class CheckSeamRuntimeTemplate extends SWTTestExt {

	private SeamPreferencesDialog seamPreferences = new SeamPreferencesDialog();
	
	protected abstract Runtime getExpectedRuntime();
	
	@Test
	public void checkSeamRuntime(){
		seamPreferences.open();
		
		assertThat(seamPreferences.getRuntimes().size(), is(1));
		assertThat(seamPreferences.getRuntimes().get(0), new RuntimeMatcher(getExpectedRuntime()));
	}
	
	@After
	public void cleanup(){
		seamPreferences.removeAllRuntimes();
		seamPreferences.ok();
	}
}
