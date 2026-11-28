import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER = Path(__file__).resolve().parents[1] / 'dialyzer_checker'
KNOWN_GUARD = "yaws.erl:10:15: Guard test Ret :: 'file' == 'fd' can never succeed"
KNOWN_CTL = 'yaws_ctl.erl:500: Function hup/1 has no local return'
UNEXPECTED = 'yaws.erl:20:1: Function unexpected/0 has no local return'


class DialyzerCheckerTest(unittest.TestCase):
    def run_checker(self, report_text):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / 'dialyzer_warnings'
            if report_text is not None:
                report.write_text(report_text, encoding='utf-8')
            result = subprocess.run([sys.executable, str(CHECKER)], cwd=directory,
                                    capture_output=True, text=True)
            return result, report.exists()

    def test_known_warnings_can_be_reordered_and_repeated(self):
        result, report_exists = self.run_checker(f'\n{KNOWN_CTL}\n{KNOWN_GUARD}\n{KNOWN_CTL}\n')
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, '')
        self.assertFalse(report_exists)

    def test_only_unexpected_warnings_are_reported(self):
        unknown_call = 'Unknown functions:  erlsom:scan/2 (yaws_soap_lib.erl:245:10)'
        result, report_exists = self.run_checker(
            f'{UNEXPECTED}\n{KNOWN_GUARD}\n{KNOWN_CTL}\n{unknown_call}\n')
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout.splitlines(), [UNEXPECTED, unknown_call])
        self.assertTrue(report_exists)

    def test_known_patterns_must_match_the_entire_warning(self):
        warnings = [f'{KNOWN_GUARD} (additional diagnostic)',
                    KNOWN_GUARD.replace('.erl', '_erl')]
        result, _ = self.run_checker('\n'.join(warnings))
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout.splitlines(), warnings)

    def test_warning_count_cannot_wrap_to_success(self):
        result, report_exists = self.run_checker(f'{UNEXPECTED}\n' * 256)
        self.assertEqual(result.returncode, 1)
        self.assertEqual(len(result.stdout.splitlines()), 256)
        self.assertTrue(report_exists)

    def test_missing_or_blank_report_is_an_error(self):
        for report_text in (None, '\n  \n'):
            with self.subTest(report_text=report_text):
                result, _ = self.run_checker(report_text)
                self.assertEqual(result.returncode, 1)
                self.assertIn('dialyzer_warnings', result.stderr)


if __name__ == '__main__':
    unittest.main()
