package org.jboss.tools.ui.bot.ext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.log4j.Logger;
import org.eclipse.swtbot.swt.finder.junit.ScreenshotCaptureListener;
import org.jboss.tools.ui.bot.ext.config.Annotations.SWTBotTestRequires;
import org.jboss.tools.ui.bot.ext.config.TestConfiguration;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;
import org.jboss.tools.ui.bot.ext.config.requirement.RequirementBase;
import org.junit.runner.Description;
import org.junit.runner.Runner;
import org.junit.runner.notification.RunListener;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.Suite;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.RunnerBuilder;
import org.junit.runners.model.Statement;

/**
 * JUnit4 requirement aware testsuite runner. If suite class is annotated by @
 * RunWith({@link RequirementAwareSuite}) class, test classes can have
 * {@link SWTBotTestRequires} annotations
 * 
 * @author lzoubek@redhat.com
 */
public class RequirementAwareSuite extends Suite {
	// we have one global instance of cleanup listener
	final static DoAfterAllTestsRunListener cleanUp = new DoAfterAllTestsRunListener();

	class ReqAwareClassRunner extends BlockJUnit4ClassRunner {
		private final TestConfiguration config;
		private final List<RequirementBase> requirements;

		public ReqAwareClassRunner(Class<?> klass,
				List<RequirementBase> requirements, TestConfiguration config)
				throws InitializationError {
			super(klass);
			this.requirements = requirements;
			this.config = config;
		}

		@Override
		public void run(RunNotifier notifier) {
			// planned test counter must know about all tests (methods) within a
			// class
			cleanUp.incrPlanned(getChildren().size() - 1);
			// ensure that we have exactly 1 cleanup listener registered
			notifier.removeListener(cleanUp);
			notifier.addListener(cleanUp);
			// adding ability to create screen shot (taken from
			// SWTBotJunit4ClassRunner)
			RunListener failureSpy = new ScreenshotCaptureListener();
			notifier.removeListener(failureSpy);
			notifier.addListener(failureSpy);
			try {
				super.run(notifier);
			} finally {
				notifier.removeListener(failureSpy);
			}
		}

		@Override
		protected String testName(FrameworkMethod method) {
			return config.getPropName() + " - " + method.getName();
		}

		@Override
		protected Statement withBeforeClasses(Statement statement) {
			if (!this.config.equals(TestConfigurator.currentConfig)) {
				TestConfigurator.currentConfig = this.config;
			}
			log.info("Fullfilling requirements before test "
					+ getTestClass().getJavaClass());
			try {
				for (RequirementBase r : requirements) {
					r.fulfill();
				}
			} catch (Exception e) {
				log.error("Fulfilling failed", e);
			}
			return super.withBeforeClasses(statement);
		}
	}

	private static final Logger log = Logger
			.getLogger(RequirementAwareSuite.class);

	private class RequirementAwareRunnerBuilder extends RunnerBuilder {
		private final TestConfiguration config;

		public RequirementAwareRunnerBuilder(TestConfiguration config) {
			this.config = config;
		}

		@Override
		public Runner runnerForClass(Class<?> klass) throws Throwable {
			if (!this.config.equals(TestConfigurator.currentConfig)) {
				TestConfigurator.currentConfig = this.config;
			}
			List<RequirementBase> reqs = TestConfigurator.getClassRequirements(klass);
			if (reqs != null) {
				SWTBotTestRequires anno = klass.getAnnotation(SWTBotTestRequires.class);
				if (anno!=null && anno.runOnce() && cleanUp.isClassPlanned(klass)) {
					// class is already planned to run and contains annotation runOnce
					log.info("Skipping class '" + klass.getCanonicalName()
					+ "' - runOnce=true, class already planned");
					return null;
				}
				log.info("Returning runner for class '"
						+ klass.getCanonicalName() + "'");
				// increment number of tests planned to run by 1 (class contains
				// at least 1 test method)
				cleanUp.incrPlanned();
				cleanUp.addClass(klass);
				return new ReqAwareClassRunner(klass, reqs, config);
			}
			log.info("Skipping class '" + klass.getCanonicalName()
					+ "' - annotations do not met configuration");
			return null;
		}

	}

	/**
	 * listener which listens to test runs, does some cleanup after all tests
	 * have run it also holds set of all classes which run (usefull for runOnce annotation)
	 * 
	 * @author lzoubek
	 * 
	 */
	static class DoAfterAllTestsRunListener extends RunListener {
		// As we can run more suites at once, we need to count tests which are
		// planned to run
		// and the ones which already passed (or failed), perform cleanups when
		// the last one finishes
		private int testsAboutToRun = 0;
		private int testsFinished = 0;

		public void incrPlanned() {
			testsAboutToRun += 1;
		}

		public void incrPlanned(int amount) {
			testsAboutToRun += amount;
		}

		public void incrFinished() {
			testsFinished += 1;
		}

		public int getPlanned() {
			return testsAboutToRun;
		}

		public int getFinished() {
			return testsFinished;
		}
		private Set<String> classes = new HashSet<String>();
		/**
		 * adds class to runList - as it is planned to run
		 * @param klass
		 */
		public void addClass(Class<?> klass) {
			classes.add(klass.getName());
		}
		public boolean isClassPlanned(Class<?> klass) {
			return classes.contains(klass.getName());
		}

		@Override
		public void testFinished(Description description) throws Exception {
			incrFinished();
			log.info("Finished test : " + description.getDisplayName());
			log.info("Finished tests : " + getFinished() + "/" + getPlanned());
			if (getFinished() >= getPlanned()) {
				log
						.info("All tests finished, performing cleanup requirements ");
				try {
					RequirementBase.createStopServer().fulfill();

					log.info("All cleanup requirements performed");
				} catch (Exception ex) {
					log.error("Unable to fulfill cleanup requirements", ex);
				}
			}
			super.testFinished(description);
		}
	}

	private final ArrayList<Runner> runners = new ArrayList<Runner>();

	/**
	 * Only called reflectively. Do not use programmatically.
	 */

	/**
	 * Called reflectively on classes annotated with
	 * <code>@RunWith(RequirementAwareSuite.class)</code>
	 * 
	 * @param klass
	 *            the root class
	 */
	public RequirementAwareSuite(Class<?> klass) throws Throwable {
		super(klass, Collections.<Runner> emptyList());
		log.info("Loading test configurations");
		for (Entry<Object, Object> entry : TestConfigurator.multiProperties
				.entrySet()) {
			try {
				TestConfiguration config = new TestConfiguration(entry.getKey()
						.toString(), entry.getValue().toString());
				String suiteName = config.getPropName() + " - "
						+ klass.getCanonicalName();
				runners.add(new NamedSuite(klass,
						new RequirementAwareRunnerBuilder(config), suiteName));
			} catch (Exception ex) {
				log.error("Error loading test configuration", ex);
			}
		}
	}

	@Override
	protected List<Runner> getChildren() {
		return runners;
	}

	public class NamedSuite extends Suite {
		private final String suiteName;

		public NamedSuite(Class<?> klass, RunnerBuilder builder, String name)
				throws InitializationError {
			super(klass, builder);
			this.suiteName = name;
		}

		@Override
		protected String getName() {
			return suiteName;
		}

	}
}
